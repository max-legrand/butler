open Types
module Int_set = Set.Make (Int)

let poll_interval = 0.1
let debounce_interval = 0.075
let stop = ref false

type watch_target =
  { node_index : int
  ; patterns : Re.re list
  }

type child =
  { pid : int
  ; name : string
  ; color : string option
  ; mutable out_fds : (Unix.file_descr * Buffer.t) list
  }

type watcher =
  { w_pid : int
  ; w_fd : Unix.file_descr
  ; w_buf : Buffer.t
  ; mutable w_pending : Int_set.t
  ; mutable w_deadline : float option
  }

let service_details graph index =
  let service = graph.nodes.(index).service in
  service.name, service.color
;;

let set_state graph index state = graph.nodes.(index).state <- state

(* ---- watch patterns ---- *)

let compile_watch_targets graph leaves =
  List.fold_right
    (fun node_index acc ->
       let* acc = acc in
       let service = graph.nodes.(node_index).service in
       let* patterns =
         List.fold_right
           (fun pattern acc ->
              let* acc = acc in
              match Re.Perl.re ("^(?:" ^ pattern ^ ")$") |> Re.compile with
              | re -> Ok (re :: acc)
              | exception _ ->
                Error (Printf.sprintf "invalid watch pattern for %s: %s" service.name pattern))
           (Option.value service.watchlist ~default:[])
           (Ok [])
       in
       if patterns = [] then Ok acc else Ok ({ node_index; patterns } :: acc))
    leaves
    (Ok [])
;;

let matching_targets paths targets root =
  let prefix = if root = "/" then root else root ^ "/" in
  let plen = String.length prefix in
  List.fold_left
    (fun affected path ->
       if String.length path > plen && String.sub path 0 plen = prefix
       then (
         let relative = String.sub path plen (String.length path - plen) in
         List.fold_left
           (fun affected target ->
              if List.exists (fun re -> Re.execp re relative) target.patterns
              then Int_set.add target.node_index affected
              else affected)
           affected
           targets)
       else affected)
    Int_set.empty
    paths
;;

(* The transitive prerequisites of [targets], in topological order. *)
let prerequisite_closure graph order targets =
  let closure = Hashtbl.create 16 in
  let rec visit index =
    List.iter
      (fun dep ->
         if not (Hashtbl.mem closure dep)
         then (
           Hashtbl.add closure dep ();
           visit dep))
      graph.nodes.(index).deps
  in
  Int_set.iter visit targets;
  List.filter (Hashtbl.mem closure) order
;;

(* ---- process handling ---- *)

let prepare_command service =
  match service.command, service.run_as_shell with
  | Command_string source, true -> Ok [ "sh"; "-c"; source ]
  | Command_string source, false ->
    (match Shell_words.split source with
     | Error error ->
       Error (Printf.sprintf "invalid command string for service %s: %s" service.name error)
     | Ok [] -> Error (Printf.sprintf "service %s has an empty command" service.name)
     | Ok args -> Ok args)
  | Command_args [], false -> Error (Printf.sprintf "service %s has an empty command" service.name)
  | Command_args args, false -> Ok args
  | Command_args _, true ->
    Error
      (Printf.sprintf
         "service %s sets run-as-shell but command is not a string"
         service.name)
;;

(* Fork and exec [argv] in a new session (and so a new process group), so the
   whole tree can be killed with one signal and the terminal's Ctrl-C does not
   reach it. [stdout] and [stderr] are the descriptors the child writes to.
   An exec failure is reported back through a close-on-exec pipe. *)
let spawn_process ~cwd ~stdout ~stderr argv =
  let err_r, err_w = Unix.pipe ~cloexec:true () in
  match Unix.fork () with
  | 0 ->
    (try
       ignore (Unix.setsid ());
       let devnull = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
       Unix.dup2 devnull Unix.stdin;
       Unix.dup2 stdout Unix.stdout;
       Unix.dup2 stderr Unix.stderr;
       Option.iter Unix.chdir cwd;
       match argv with
       | program :: _ -> Unix.execvp program (Array.of_list argv)
       | [] -> ()
     with
     | Unix.Unix_error (e, fn, arg) ->
       let message = Printf.sprintf "%s: %s %s" (Unix.error_message e) fn arg in
       ignore (Unix.write_substring err_w message 0 (String.length message))
     | _ -> ());
    Unix._exit 127
  | pid ->
    Unix.close err_w;
    let buf = Bytes.create 512 in
    let n = try Unix.read err_r buf 0 512 with Unix.Unix_error _ -> 0 in
    Unix.close err_r;
    if n = 0
    then Ok pid
    else (
      ignore (Unix.waitpid [] pid);
      Error (Bytes.sub_string buf 0 n))
;;

let spawn_service graph index =
  let service = graph.nodes.(index).service in
  let* argv = prepare_command service in
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let err_r, err_w = Unix.pipe ~cloexec:true () in
  let result = spawn_process ~cwd:service.cwd ~stdout:out_w ~stderr:err_w argv in
  Unix.close out_w;
  Unix.close err_w;
  match result with
  | Ok pid ->
    Ok
      { pid
      ; name = service.name
      ; color = service.color
      ; out_fds = [ out_r, Buffer.create 256; err_r, Buffer.create 256 ]
      }
  | Error message ->
    Unix.close out_r;
    Unix.close err_r;
    Error (Printf.sprintf "failed to start service %s: %s" service.name message)
;;

let emit_lines child buf ~final =
  let data = Buffer.contents buf in
  let lines = String.split_on_char '\n' data in
  let rec go = function
    | [] -> ()
    | [ last ] ->
      Buffer.clear buf;
      if final && last <> ""
      then Logger.service child.name child.color last
      else Buffer.add_string buf last
    | line :: rest ->
      let line =
        if line <> "" && line.[String.length line - 1] = '\r'
        then String.sub line 0 (String.length line - 1)
        else line
      in
      Logger.service child.name child.color line;
      go rest
  in
  go lines
;;

let scratch = Bytes.create 4096

(* Read what is available on [fd] into [buf]. Returns false at end of file. *)
let read_into fd buf =
  match Unix.read fd scratch 0 (Bytes.length scratch) with
  | 0 -> false
  | n ->
    Buffer.add_subbytes buf scratch 0 n;
    true
  | exception Unix.Unix_error ((Unix.EINTR | Unix.EAGAIN), _, _) -> true
  | exception Unix.Unix_error _ -> false
;;

let handle_child_readable child ready =
  child.out_fds
  <- List.filter
       (fun (fd, buf) ->
          if List.mem fd ready
          then (
            let alive = read_into fd buf in
            emit_lines child buf ~final:(not alive);
            if not alive then Unix.close fd;
            alive)
          else true)
       child.out_fds
;;

let select_read fds timeout =
  match Unix.select fds [] [] timeout with
  | ready, _, _ -> ready
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> []
;;

(* Kill the child's whole process group, reap it, and flush its output. *)
let terminate_child child =
  (try Unix.kill (-child.pid) Sys.sigkill with
   | Unix.Unix_error (Unix.ESRCH, _, _) -> ()
   | Unix.Unix_error (Unix.EPERM, _, _) -> ());
  (try ignore (Unix.waitpid [] child.pid) with
   | Unix.Unix_error (Unix.ECHILD, _, _) -> ());
  List.iter
    (fun (fd, buf) ->
       let rec drain () =
         if select_read [ fd ] 0.0 <> [] && read_into fd buf then drain ()
       in
       drain ();
       emit_lines child buf ~final:true;
       Unix.close fd)
    child.out_fds;
  child.out_fds <- []
;;

let try_wait child =
  match Unix.waitpid [ Unix.WNOHANG ] child.pid with
  | 0, _ -> None
  | _, status -> Some status
  | exception Unix.Unix_error (Unix.ECHILD, _, _) -> Some (Unix.WEXITED 0)
;;

let status_success = function
  | Unix.WEXITED 0 -> true
  | _ -> false
;;

let describe_status = function
  | Unix.WEXITED n -> Printf.sprintf "exit status: %d" n
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> Printf.sprintf "signal: %d" n
;;

(* ---- prerequisites ---- *)

(* Run each prerequisite to completion in order. Returns Ok true when stopped
   by a signal. *)
let run_prerequisites graph indices =
  let rec each = function
    | [] -> Ok false
    | _ when !stop -> Ok true
    | index :: rest ->
      let name, color = service_details graph index in
      set_state graph index Ready;
      (match spawn_service graph index with
       | Error error ->
         set_state graph index Failed;
         Error error
       | Ok child ->
         set_state graph index Running;
         Logger.service name color "started prerequisite";
         let rec wait () =
           if !stop
           then (
             terminate_child child;
             Ok true)
           else (
             let ready = select_read (List.map fst child.out_fds) poll_interval in
             handle_child_readable child ready;
             match try_wait child with
             | None -> wait ()
             | Some status ->
               terminate_child child;
               if status_success status
               then (
                 set_state graph index Succeeded;
                 Logger.service name color "prerequisite completed";
                 each rest)
               else (
                 set_state graph index Failed;
                 Error
                   (Printf.sprintf
                      "prerequisite %s exited unsuccessfully (%s)"
                      name
                      (describe_status status))))
         in
         wait ())
  in
  each indices
;;

(* ---- watchexec ---- *)

let start_watchexec () =
  let out_r, out_w = Unix.pipe ~cloexec:true () in
  let argv =
    [ "watchexec"
    ; "--only-emit-events"
    ; "--emit-events-to=json-stdio"
    ; "--ignore-nothing"
    ; "--fs-events"
    ; "create,remove,rename,modify"
    ]
  in
  let result = spawn_process ~cwd:None ~stdout:out_w ~stderr:Unix.stderr argv in
  Unix.close out_w;
  match result with
  | Ok w_pid ->
    Ok
      { w_pid
      ; w_fd = out_r
      ; w_buf = Buffer.create 1024
      ; w_pending = Int_set.empty
      ; w_deadline = None
      }
  | Error message ->
    Unix.close out_r;
    Error
      (Printf.sprintf
         "failed to start watchexec (is it installed and on PATH?): %s"
         message)
;;

let is_content_change full =
  let starts p =
    String.length full >= String.length p && String.sub full 0 (String.length p) = p
  in
  starts "Create" || starts "Remove" || starts "Modify(Data" || starts "Modify(Name"
;;

(* The paths of a watchexec JSON event, if it is a file content change. *)
let event_paths line =
  match Yojson.Safe.from_string line with
  | exception _ -> []
  | `Assoc fields ->
    let tags =
      match List.assoc_opt "tags" fields with
      | Some (`List tags) -> tags
      | _ -> []
    in
    let field name tag =
      match tag with
      | `Assoc f ->
        (match List.assoc_opt name f with
         | Some (`String s) -> Some s
         | _ -> None)
      | _ -> None
    in
    let is_content =
      List.exists
        (fun tag ->
           field "kind" tag = Some "fs"
           && Option.fold ~none:false ~some:is_content_change (field "full" tag))
        tags
    in
    if is_content
    then
      List.filter_map
        (fun tag -> if field "kind" tag = Some "path" then field "absolute" tag else None)
        tags
    else []
  | _ -> []
;;

(* Read watchexec output; returns false when its stream has closed. *)
let handle_watcher_readable watcher targets root =
  let alive = read_into watcher.w_fd watcher.w_buf in
  let data = Buffer.contents watcher.w_buf in
  Buffer.clear watcher.w_buf;
  let lines = String.split_on_char '\n' data in
  let rec go = function
    | [] -> ()
    | [ partial ] -> Buffer.add_string watcher.w_buf partial
    | line :: rest ->
      let affected = matching_targets (event_paths line) targets root in
      if not (Int_set.is_empty affected)
      then (
        watcher.w_pending <- Int_set.union watcher.w_pending affected;
        if watcher.w_deadline = None
        then watcher.w_deadline <- Some (Unix.gettimeofday () +. debounce_interval));
      go rest
  in
  go lines;
  alive
;;

let stop_watcher watcher =
  (try Unix.kill (-watcher.w_pid) Sys.sigkill with
   | Unix.Unix_error _ -> ());
  (try ignore (Unix.waitpid [] watcher.w_pid) with
   | Unix.Unix_error _ -> ());
  Unix.close watcher.w_fd
;;

(* ---- main loop ---- *)

let start_service graph index children =
  set_state graph index Ready;
  match spawn_service graph index with
  | Error error ->
    set_state graph index Failed;
    Error error
  | Ok child ->
    set_state graph index Running;
    let name, color = service_details graph index in
    Logger.service name color "started";
    Hashtbl.replace children index child;
    Ok ()
;;

let poll_children graph children =
  Hashtbl.fold (fun index child acc -> (index, child) :: acc) children []
  |> List.iter (fun (index, child) ->
    match try_wait child with
    | None -> ()
    | Some status ->
      Hashtbl.remove children index;
      terminate_child child;
      set_state graph index (if status_success status then Succeeded else Failed);
      let name, color = service_details graph index in
      Logger.service name color (Printf.sprintf "exited (%s)" (describe_status status)))
;;

let stop_children graph children =
  Hashtbl.iter
    (fun index child ->
       terminate_child child;
       set_state graph index Pending)
    children;
  Hashtbl.reset children
;;

let report = function
  | Ok () -> ()
  | Error message -> Logger.error message
;;

let run graph =
  let* order = Graph.topological_sort graph in
  let partition = Graph.partition_helper graph in
  let prerequisite_order = List.filter (fun i -> List.mem i partition.prerequisites) order in
  let* targets = compile_watch_targets graph partition.services in
  let has_watch_targets = targets <> [] in
  stop := false;
  List.iter
    (fun signal -> Sys.set_signal signal (Sys.Signal_handle (fun _ -> stop := true)))
    [ Sys.sigint; Sys.sigterm; Sys.sighup ];
  let* stopped = run_prerequisites graph prerequisite_order in
  if stopped
  then Ok ()
  else (
    let root = try Unix.realpath (Sys.getcwd ()) with Unix.Unix_error _ -> Sys.getcwd () in
    let* watcher =
      if has_watch_targets
      then
        let* w = start_watchexec () in
        Ok (Some w)
      else Ok None
    in
    let children = Hashtbl.create 16 in
    List.iter
      (fun target -> if not !stop then report (start_service graph target children))
      partition.services;
    let rec loop () =
      if !stop
      then ()
      else (
        poll_children graph children;
        if (not has_watch_targets) && Hashtbl.length children = 0
        then ()
        else (
          let child_fds =
            Hashtbl.fold (fun _ c acc -> List.map fst c.out_fds @ acc) children []
          in
          let watcher_fds =
            match watcher with
            | Some w -> [ w.w_fd ]
            | None -> []
          in
          let timeout =
            match Option.bind watcher (fun w -> w.w_deadline) with
            | Some deadline -> Float.max 0.0 (Float.min poll_interval (deadline -. Unix.gettimeofday ()))
            | None -> poll_interval
          in
          let ready = select_read (watcher_fds @ child_fds) timeout in
          Hashtbl.iter (fun _ c -> handle_child_readable c ready) children;
          let watcher_open =
            match watcher with
            | Some w when List.mem w.w_fd ready -> handle_watcher_readable w targets root
            | _ -> true
          in
          if not watcher_open
          then Logger.error "Watchexec event channel closed"
          else (
            (match watcher with
             | Some ({ w_deadline = Some deadline; _ } as w)
               when Unix.gettimeofday () >= deadline ->
               let affected = w.w_pending in
               w.w_pending <- Int_set.empty;
               w.w_deadline <- None;
               restart graph order children affected
             | _ -> ());
            loop ())))
    and restart graph order children affected =
      let rerun_order = prerequisite_closure graph order affected in
      match run_prerequisites graph rerun_order with
      | Ok true -> ()
      | Error error ->
        Logger.error (Printf.sprintf "watch-triggered prerequisite run failed: %s" error)
      | Ok false ->
        List.iter
          (fun target ->
             if Int_set.mem target affected && not !stop
             then (
               Option.iter
                 (fun child ->
                    Hashtbl.remove children target;
                    terminate_child child)
                 (Hashtbl.find_opt children target);
               report (start_service graph target children)))
          order
    in
    loop ();
    stop_children graph children;
    Option.iter stop_watcher watcher;
    Ok ())
;;
