//! Service process execution and file-watch restarts.

use crate::graph::{partition_helper, topological_sort};
use crate::logger::Logger;
use crate::types::{CommandSpec, Graph, State};
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::io::{BufRead, BufReader};
#[cfg(unix)]
use std::os::unix::process::CommandExt;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, mpsc};
use std::thread;
use std::time::Duration;
use tokio::runtime::Builder;
use watchexec::Watchexec;
use watchexec::error::RuntimeError;
use watchexec::filter::Filterer;
use watchexec_events::filekind::{FileEventKind, ModifyKind};
use watchexec_events::{Event, Priority, Tag};
use watchexec_signals::Signal;

const POLL_INTERVAL: Duration = Duration::from_millis(100);
const DEBOUNCE_INTERVAL: Duration = Duration::from_millis(75);

#[derive(Debug)]
struct WatchTarget {
    node_index: usize,
    patterns: Vec<Regex>,
}

struct WatchexecHandle {
    events: mpsc::Receiver<HashSet<usize>>,
    thread: thread::JoinHandle<Result<(), String>>,
}

struct ServiceChild {
    process: Child,
    output_threads: Vec<thread::JoinHandle<()>>,
}

#[derive(Debug)]
struct ContentChangeFilterer;

impl Filterer for ContentChangeFilterer {
    fn check_dir(&self, _path: &Path) -> Result<bool, RuntimeError> {
        Ok(true)
    }

    fn check_event(&self, event: &Event, _priority: Priority) -> Result<bool, RuntimeError> {
        Ok(event.tags.iter().any(|tag| {
            matches!(
                tag,
                Tag::FileEventKind(
                    FileEventKind::Create(_)
                        | FileEventKind::Remove(_)
                        | FileEventKind::Modify(ModifyKind::Data(_) | ModifyKind::Name(_))
                )
            )
        }))
    }
}

/// Run prerequisite commands, then supervise leaf services and restart them on watched changes.
///
/// # Errors
///
/// Returns an error when graph ordering, watcher setup, signal setup, or initial
/// prerequisite execution fails.
pub fn run(graph: &mut Graph, logger: Logger) -> Result<(), String> {
    let order = topological_sort(graph)?;
    let partition = partition_helper(graph);
    let prerequisite_set: HashSet<usize> = partition.prerequisites.into_iter().collect();
    let prerequisite_order: Vec<usize> = order
        .iter()
        .copied()
        .filter(|index| prerequisite_set.contains(index))
        .collect();
    let targets = compile_watch_targets(graph, &partition.services)?;
    let has_watch_targets = !targets.is_empty();
    let stop = Arc::new(AtomicBool::new(false));

    if run_prerequisites(graph, &prerequisite_order, &stop, logger)? {
        return Ok(());
    }

    let watcher = if has_watch_targets {
        let root = std::env::current_dir().map_err(|error| error.to_string())?;
        Some(start_watchexec(targets, root, Arc::clone(&stop))?)
    } else {
        None
    };

    let mut children = HashMap::new();
    for target in &partition.services {
        if stop.load(Ordering::SeqCst) {
            break;
        }
        if let Err(error) = start_service(graph, *target, &mut children, logger) {
            logger.error(&error);
        }
    }

    loop {
        if stop.load(Ordering::SeqCst) {
            break;
        }
        poll_children(graph, &mut children, logger);

        if !has_watch_targets {
            if children.is_empty() {
                break;
            }
            thread::sleep(POLL_INTERVAL);
            continue;
        }

        let Some(watcher) = watcher.as_ref() else {
            break;
        };
        match watcher.events.recv_timeout(POLL_INTERVAL) {
            Ok(affected) => {
                if affected.is_empty() {
                    continue;
                }
                let rerun_order = prerequisite_closure(graph, &order, &affected);
                match run_prerequisites(graph, &rerun_order, &stop, logger) {
                    Ok(true) => break,
                    Err(error) => {
                        logger.error(format!("watch-triggered prerequisite run failed: {error}"));
                        continue;
                    }
                    Ok(false) => {}
                }
                for target in order.iter().filter(|index| affected.contains(index)) {
                    if stop.load(Ordering::SeqCst) {
                        break;
                    }
                    if let Some(mut child) = children.remove(target)
                        && let Err(error) = terminate_child(&mut child)
                    {
                        logger.error(format!("failed to stop service before restart: {error}"));
                        continue;
                    }
                    if let Err(error) = start_service(graph, *target, &mut children, logger) {
                        logger.error(&error);
                    }
                }
            }
            Err(mpsc::RecvTimeoutError::Timeout) => {}
            Err(mpsc::RecvTimeoutError::Disconnected) => {
                logger.error("Watchexec event channel closed");
                break;
            }
        }
    }

    stop_children(graph, &mut children, logger);
    if let Some(watcher) = watcher {
        match watcher.thread.join() {
            Ok(Ok(())) => {}
            Ok(Err(error)) => logger.error(format!("Watchexec stopped: {error}")),
            Err(error) => logger.error(format!("Watchexec thread panicked: {error:?}")),
        }
    }
    Ok(())
}

fn compile_watch_targets(graph: &Graph, leaves: &[usize]) -> Result<Vec<WatchTarget>, String> {
    let mut targets = Vec::new();
    for &node_index in leaves {
        let Some(node) = graph.nodes.get(node_index) else {
            return Err("invalid graph node index".to_owned());
        };
        let patterns = node
            .service
            .watchlist
            .as_deref()
            .unwrap_or_default()
            .iter()
            .map(|pattern| {
                let anchored = format!("^(?:{pattern})$");
                Regex::new(&anchored).map_err(|error| {
                    format!("invalid watch pattern for {}: {error}", node.service.name)
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        if !patterns.is_empty() {
            targets.push(WatchTarget {
                node_index,
                patterns,
            });
        }
    }
    Ok(targets)
}

fn start_watchexec(
    targets: Vec<WatchTarget>,
    root: PathBuf,
    stop: Arc<AtomicBool>,
) -> Result<WatchexecHandle, String> {
    let (event_sender, event_receiver) = mpsc::channel();
    let (ready_sender, ready_receiver) = mpsc::sync_channel(1);
    let watcher_thread = thread::spawn(move || {
        let runtime = match Builder::new_current_thread().enable_all().build() {
            Ok(runtime) => runtime,
            Err(error) => {
                let message = format!("failed to create Watchexec runtime: {error}");
                let _ = ready_sender.send(Err(message.clone()));
                return Err(message);
            }
        };
        runtime.block_on(async move {
            let action_root = root.clone();
            let action_stop = Arc::clone(&stop);
            let action_sender = event_sender;
            let watcher = match Watchexec::new(move |mut action| {
                if action
                    .signals()
                    .any(|signal| matches!(signal, Signal::Interrupt | Signal::Terminate))
                {
                    action_stop.store(true, Ordering::SeqCst);
                    action.quit();
                    return action;
                }
                let affected =
                    matching_targets(action.paths().map(|(path, _)| path), &targets, &action_root);
                if !affected.is_empty() {
                    let _ = action_sender.send(affected);
                }
                action
            }) {
                Ok(watcher) => watcher,
                Err(error) => {
                    let message = format!("failed to create Watchexec: {error}");
                    let _ = ready_sender.send(Err(message.clone()));
                    return Err(message);
                }
            };
            watcher.config.pathset([root]);
            watcher.config.filterer(ContentChangeFilterer);
            watcher.config.throttle(DEBOUNCE_INTERVAL);
            let main = watcher.main();
            let _ = ready_sender.send(Ok(()));
            main.await
                .map_err(|error| format!("Watchexec task failed: {error}"))?
                .map_err(|error| format!("Watchexec failed: {error}"))
        })
    });

    match ready_receiver.recv() {
        Ok(Ok(())) => Ok(WatchexecHandle {
            events: event_receiver,
            thread: watcher_thread,
        }),
        Ok(Err(error)) => {
            let _ = watcher_thread.join();
            Err(error)
        }
        Err(error) => {
            let _ = watcher_thread.join();
            Err(format!("Watchexec startup failed: {error}"))
        }
    }
}

fn matching_targets<'path>(
    paths: impl Iterator<Item = &'path Path>,
    targets: &[WatchTarget],
    root: &Path,
) -> HashSet<usize> {
    let mut affected = HashSet::new();
    for path in paths {
        let Ok(relative) = path.strip_prefix(root) else {
            continue;
        };
        let path_text = relative.to_string_lossy();
        for target in targets {
            if target
                .patterns
                .iter()
                .any(|pattern| pattern.is_match(&path_text))
            {
                affected.insert(target.node_index);
            }
        }
    }
    affected
}

fn run_prerequisites(
    graph: &mut Graph,
    indices: &[usize],
    stop: &AtomicBool,
    logger: Logger,
) -> Result<bool, String> {
    for &index in indices {
        if stop.load(Ordering::SeqCst) {
            return Ok(true);
        }
        let (service_name, service_color) = service_details(graph, index)?;
        set_state(graph, index, State::Ready)?;
        let mut child = match spawn_service(graph, index, logger) {
            Ok(child) => child,
            Err(error) => {
                set_state(graph, index, State::Failed)?;
                return Err(error);
            }
        };
        set_state(graph, index, State::Running)?;
        logger.service(
            &service_name,
            service_color.as_deref(),
            "started prerequisite",
        );
        loop {
            if stop.load(Ordering::SeqCst) {
                terminate_child(&mut child)?;
                return Ok(true);
            }
            if let Some(status) = child
                .process
                .try_wait()
                .map_err(|error| format!("failed to check prerequisite {service_name}: {error}"))?
            {
                terminate_child(&mut child)?;
                if status.success() {
                    set_state(graph, index, State::Succeeded)?;
                    logger.service(
                        &service_name,
                        service_color.as_deref(),
                        "prerequisite completed",
                    );
                    break;
                }
                set_state(graph, index, State::Failed)?;
                return Err(format!(
                    "prerequisite {service_name} exited unsuccessfully ({status})"
                ));
            }
            thread::sleep(POLL_INTERVAL);
        }
    }
    Ok(false)
}

fn prerequisite_closure(graph: &Graph, order: &[usize], targets: &HashSet<usize>) -> Vec<usize> {
    let mut closure = HashSet::new();
    let mut pending: Vec<usize> = targets.iter().copied().collect();
    while let Some(index) = pending.pop() {
        let Some(node) = graph.nodes.get(index) else {
            continue;
        };
        for &dependency in &node.deps {
            if closure.insert(dependency) {
                pending.push(dependency);
            }
        }
    }
    order
        .iter()
        .copied()
        .filter(|index| closure.contains(index))
        .collect()
}

fn start_service(
    graph: &mut Graph,
    index: usize,
    children: &mut HashMap<usize, ServiceChild>,
    logger: Logger,
) -> Result<(), String> {
    set_state(graph, index, State::Ready)?;
    let child = match spawn_service(graph, index, logger) {
        Ok(child) => child,
        Err(error) => {
            set_state(graph, index, State::Failed)?;
            return Err(error);
        }
    };
    set_state(graph, index, State::Running)?;
    let (name, color) = service_details(graph, index)?;
    logger.service(&name, color.as_deref(), "started");
    children.insert(index, child);
    Ok(())
}

fn spawn_service(graph: &Graph, index: usize, logger: Logger) -> Result<ServiceChild, String> {
    let Some(node) = graph.nodes.get(index) else {
        return Err("invalid graph node index".to_owned());
    };
    let service = &node.service;
    let mut command = match (&service.command, service.run_as_shell) {
        (CommandSpec::String(source), true) => shell_command(source),
        (CommandSpec::String(source), false) => {
            let arguments = shell_words::split(source).map_err(|error| {
                format!(
                    "invalid command string for service {}: {error}",
                    service.name
                )
            })?;
            direct_command(&arguments, &service.name)?
        }
        (CommandSpec::Args(arguments), false) => direct_command(arguments, &service.name)?,
        (CommandSpec::Args(_), true) => {
            return Err(format!(
                "service {} sets run-as-shell but command is not a string",
                service.name
            ));
        }
    };
    #[cfg(unix)]
    command.process_group(0);
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut process = command
        .spawn()
        .map_err(|error| format!("failed to start service {}: {error}", service.name))?;
    let service_name = service.name.clone();
    let service_color = service.color.clone();
    let mut output_threads = Vec::new();
    if let Some(stdout) = process.stdout.take() {
        output_threads.push(log_output(
            stdout,
            service_name.clone(),
            service_color.clone(),
            logger,
        ));
    }
    if let Some(stderr) = process.stderr.take() {
        output_threads.push(log_output(stderr, service_name, service_color, logger));
    }
    Ok(ServiceChild {
        process,
        output_threads,
    })
}

fn log_output<R: std::io::Read + Send + 'static>(
    stream: R,
    service_name: String,
    service_color: Option<String>,
    logger: Logger,
) -> thread::JoinHandle<()> {
    thread::spawn(move || {
        for line in BufReader::new(stream).lines() {
            match line {
                Ok(line) => logger.service(&service_name, service_color.as_deref(), line),
                Err(error) => {
                    logger.service(
                        &service_name,
                        service_color.as_deref(),
                        format!("failed to read service output: {error}"),
                    );
                    break;
                }
            }
        }
    })
}

fn direct_command(arguments: &[String], service_name: &str) -> Result<Command, String> {
    let Some((program, arguments)) = arguments.split_first() else {
        return Err(format!("service {service_name} has an empty command"));
    };
    let mut command = Command::new(program);
    command.args(arguments);
    Ok(command)
}

fn shell_command(source: &str) -> Command {
    #[cfg(windows)]
    {
        let mut command = Command::new("cmd.exe");
        command.arg("/C").arg(source);
        command
    }
    #[cfg(not(windows))]
    {
        let mut command = Command::new("sh");
        command.arg("-c").arg(source);
        command
    }
}

fn poll_children(graph: &mut Graph, children: &mut HashMap<usize, ServiceChild>, logger: Logger) {
    let indices: Vec<usize> = children.keys().copied().collect();
    for index in indices {
        let status = match children.get_mut(&index) {
            Some(child) => child.process.try_wait(),
            None => continue,
        };
        match status {
            Ok(Some(status)) => {
                let state = if status.success() {
                    State::Succeeded
                } else {
                    State::Failed
                };
                if let Some(mut child) = children.remove(&index)
                    && let Err(error) = terminate_child(&mut child)
                {
                    logger.warn(format!(
                        "failed to stop descendants of service {index}: {error}"
                    ));
                }
                if let Err(error) = set_state(graph, index, state) {
                    logger.error(error);
                }
                match service_details(graph, index) {
                    Ok((name, color)) => {
                        logger.service(name, color.as_deref(), format!("exited ({status})"));
                    }
                    Err(error) => logger.error(error),
                }
            }
            Ok(None) => {}
            Err(error) => logger.error(format!("failed to poll service {index}: {error}")),
        }
    }
}

fn terminate_child(child: &mut ServiceChild) -> Result<(), String> {
    #[cfg(unix)]
    {
        let process_id = i32::try_from(child.process.id()).map_err(|error| error.to_string())?;
        if let Err(error) = nix::sys::signal::killpg(
            nix::unistd::Pid::from_raw(process_id),
            nix::sys::signal::Signal::SIGKILL,
        ) && error != nix::errno::Errno::ESRCH
        {
            return Err(error.to_string());
        }
    }
    if child
        .process
        .try_wait()
        .map_err(|error| error.to_string())?
        .is_none()
    {
        #[cfg(not(unix))]
        child.process.kill().map_err(|error| error.to_string())?;
        child.process.wait().map_err(|error| error.to_string())?;
    }
    for output_thread in child.output_threads.drain(..) {
        let _ = output_thread.join();
    }
    Ok(())
}

fn stop_children(graph: &mut Graph, children: &mut HashMap<usize, ServiceChild>, logger: Logger) {
    for (index, mut child) in children.drain() {
        if let Err(error) = terminate_child(&mut child) {
            logger.warn(format!("failed to stop service {index}: {error}"));
        }
        if let Err(error) = set_state(graph, index, State::Pending) {
            logger.error(error);
        }
    }
}

fn service_details(graph: &Graph, index: usize) -> Result<(String, Option<String>), String> {
    graph
        .nodes
        .get(index)
        .map(|node| (node.service.name.clone(), node.service.color.clone()))
        .ok_or_else(|| "invalid graph node index".to_owned())
}

fn set_state(graph: &mut Graph, index: usize, state: State) -> Result<(), String> {
    let Some(node) = graph.nodes.get_mut(index) else {
        return Err("invalid graph node index".to_owned());
    };
    node.state = state;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{ContentChangeFilterer, prerequisite_closure};
    use crate::graph::{generate_graph, topological_sort};
    use crate::types::{CommandSpec, ServiceSchema};
    use std::collections::HashSet;
    use watchexec::filter::Filterer;
    use watchexec_events::filekind::{
        AccessKind, AccessMode, CreateKind, DataChange, FileEventKind, MetadataKind, ModifyKind,
        RemoveKind, RenameMode,
    };
    use watchexec_events::{Event, Priority, Tag};

    fn file_event(kind: FileEventKind) -> Event {
        Event {
            tags: vec![Tag::FileEventKind(kind)],
            ..Event::default()
        }
    }

    fn service(name: &str, dependencies: &[&str]) -> ServiceSchema {
        ServiceSchema {
            name: name.to_owned(),
            command: CommandSpec::Args(vec!["dummy".to_owned()]),
            run_as_shell: false,
            color: None,
            dependencies: Some(dependencies.iter().map(|name| (*name).to_owned()).collect()),
            watchlist: None,
        }
    }

    #[test]
    fn content_filter_rejects_open_and_attribute_events() {
        let filter = ContentChangeFilterer;
        let opened = file_event(FileEventKind::Access(AccessKind::Open(AccessMode::Any)));
        let attributed = file_event(FileEventKind::Modify(ModifyKind::Metadata(
            MetadataKind::Any,
        )));
        assert!(matches!(
            filter.check_event(&opened, Priority::Normal),
            Ok(false)
        ));
        assert!(matches!(
            filter.check_event(&attributed, Priority::Normal),
            Ok(false)
        ));
        assert!(matches!(
            filter.check_event(&Event::default(), Priority::Normal),
            Ok(false)
        ));
    }

    #[test]
    fn content_filter_accepts_file_content_and_path_changes() {
        let filter = ContentChangeFilterer;
        let changes = [
            file_event(FileEventKind::Create(CreateKind::File)),
            file_event(FileEventKind::Remove(RemoveKind::File)),
            file_event(FileEventKind::Modify(ModifyKind::Data(DataChange::Content))),
            file_event(FileEventKind::Modify(ModifyKind::Name(RenameMode::To))),
        ];
        assert!(
            changes
                .iter()
                .all(|event| matches!(filter.check_event(event, Priority::Normal), Ok(true)))
        );
    }

    #[test]
    fn watch_patterns_are_anchored_without_changing_alternatives() {
        let mut watched = service("watch", &[]);
        watched.watchlist = Some(vec!["foo|bar".to_owned()]);
        let graph_result = generate_graph(&[watched]);
        assert!(graph_result.is_ok());
        let Some(graph) = graph_result.ok() else {
            return;
        };
        let targets_result = super::compile_watch_targets(&graph, &[0]);
        assert!(targets_result.is_ok());
        let Some(targets) = targets_result.ok() else {
            return;
        };
        let Some(pattern) = targets.first().and_then(|target| target.patterns.first()) else {
            return;
        };
        assert!(pattern.is_match("foo"));
        assert!(pattern.is_match("bar"));
        assert!(!pattern.is_match("nested/foo/file.txt"));
    }

    #[test]
    fn watcher_restart_includes_transitive_prerequisites_once_in_topological_order() {
        let services = vec![
            service("base", &[]),
            service("middle", &["base"]),
            service("leaf-a", &["middle"]),
            service("leaf-b", &["base"]),
        ];
        let graph_result = generate_graph(&services);
        assert!(graph_result.is_ok());
        let Some(graph) = graph_result.ok() else {
            return;
        };
        let order_result = topological_sort(&graph);
        assert!(order_result.is_ok());
        let Some(order) = order_result.ok() else {
            return;
        };
        let targets: HashSet<usize> = std::iter::once(2).collect();
        assert_eq!(prerequisite_closure(&graph, &order, &targets), vec![0, 1]);
    }
}
