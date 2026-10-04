//! Dependency graph construction.

use crate::types::{Graph, Node, ServiceSchema, State};
use std::collections::{HashMap, VecDeque};

/// An empty graph with no nodes.
#[must_use]
pub fn default_graph() -> Graph {
    Graph {
        nodes: Vec::new(),
        lookup: HashMap::new(),
    }
}

/// Build the dependency graph of `raw_services`.
///
/// Missing `dependencies` and `watchlist` fields are replaced by empty lists,
/// and every node starts in [`State::Pending`].
///
/// # Errors
///
/// Returns a message when two services share a name, or when a service
/// depends on a name that is not in the configuration.
pub fn generate_graph(raw_services: &[ServiceSchema]) -> Result<Graph, String> {
    let services: Vec<ServiceSchema> = raw_services
        .iter()
        .map(|service| ServiceSchema {
            dependencies: Some(service.dependencies.clone().unwrap_or_default()),
            watchlist: Some(service.watchlist.clone().unwrap_or_default()),
            ..service.clone()
        })
        .collect();

    let mut lookup = HashMap::new();
    for (index, service) in services.iter().enumerate() {
        if lookup.insert(service.name.clone(), index).is_some() {
            return Err(format!("duplicate service: {}", service.name));
        }
    }

    let mut nodes = Vec::with_capacity(services.len());
    for service in &services {
        let mut deps = Vec::new();
        for dependency in service.dependencies.as_deref().unwrap_or_default() {
            match lookup.get(dependency) {
                Some(index) => deps.push(*index),
                None => {
                    return Err(format!(
                        "unknown dependency for service {}: {}",
                        service.name, dependency
                    ));
                }
            }
        }
        nodes.push(Node {
            service: service.clone(),
            deps,
            dependents: Vec::new(),
            state: State::Pending,
        });
    }

    for index in 0..nodes.len() {
        let deps = match nodes.get(index) {
            Some(node) => node.deps.clone(),
            None => return Err("Failed to properly build graph".to_owned()),
        };
        for dependency in deps {
            match nodes.get_mut(dependency) {
                Some(node) => node.dependents.push(index),
                None => return Err("Failed to properly build graph".to_owned()),
            }
        }
    }

    Ok(Graph { nodes, lookup })
}

/// Order the graph's nodes so that every service follows its dependencies.
///
/// The result is a permutation of the graph's nodes: every service appears
/// exactly once, and every service sits after all the services it depends on.
/// Services without dependencies come first. Ties are broken by configuration
/// position, so the same graph always yields the same order.
///
/// # Errors
///
/// Returns a message naming the services involved when the graph contains a
/// dependency cycle, because a cyclic graph has no valid order.
pub fn topological_sort(graph: &Graph) -> Result<Vec<usize>, String> {
    let mut in_degree: Vec<usize> = graph.nodes.iter().map(|node| node.deps.len()).collect();
    let mut queue: VecDeque<usize> = graph
        .nodes
        .iter()
        .enumerate()
        .filter_map(|(index, node)| {
            if node.deps.is_empty() {
                Some(index)
            } else {
                None
            }
        })
        .collect();

    let mut order = Vec::with_capacity(graph.nodes.len());

    while let Some(index) = queue.pop_front() {
        order.push(index);
        let Some(node) = graph.nodes.get(index) else {
            return Err("Failed to properly build graph".to_owned());
        };
        for &dependent in &node.dependents {
            let Some(degree) = in_degree.get_mut(dependent) else {
                return Err("Failed to properly build graph".to_owned());
            };
            let Some(next) = degree.checked_sub(1) else {
                return Err("Failed to properly build graph".to_owned());
            };
            *degree = next;
            if next == 0 {
                queue.push_back(dependent);
            }
        }
    }

    if order.len() != graph.nodes.len() {
        let mut blocked: Vec<&str> = Vec::new();
        for (index, node) in graph.nodes.iter().enumerate() {
            let Some(degree) = in_degree.get(index) else {
                return Err("Failed to properly build graph".to_owned());
            };
            if *degree > 0 {
                blocked.push(node.service.name.as_str());
            }
        }
        blocked.sort_unstable();
        return Err(format!("dependency cycle: {}", blocked.join(", ")));
    }

    Ok(order)
}

#[derive(Default, Debug)]
pub struct Partition {
    pub prerequisites: Vec<usize>,
    pub services: Vec<usize>,
}

#[must_use]
pub fn partition_helper(graph: &Graph) -> Partition {
    let mut partition = Partition::default();
    for (idx, node) in graph.nodes.iter().enumerate() {
        if node.dependents.is_empty() {
            partition.services.push(idx);
        } else {
            partition.prerequisites.push(idx);
        }
    }
    partition
}
