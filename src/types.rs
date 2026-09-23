//! Core types shared by the Butler library.

use serde::Deserialize;
use std::collections::HashMap;

/// The command representation used in a service configuration.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(untagged)]
pub enum CommandSpec {
    /// A single command string.
    String(String),
    /// An executable followed by its arguments.
    Args(Vec<String>),
}

/// A service as described in the service configuration file.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct ServiceSchema {
    /// Unique name of the service.
    pub name: String,
    /// Command string or executable and its arguments.
    pub command: CommandSpec,
    /// Run a string command through the platform shell. Defaults to `false`.
    #[serde(rename = "run-as-shell", default)]
    pub run_as_shell: bool,
    /// Optional display color, for example `#74ACDF`.
    pub color: Option<String>,
    /// Names of the services this service waits for.
    #[serde(default)]
    pub dependencies: Option<Vec<String>>,
    /// Optional regex patterns matched against working-directory-relative paths.
    /// A matching change reruns this leaf service's prerequisites before the service.
    #[serde(default)]
    pub watchlist: Option<Vec<String>>,
}

/// Semantic version of Butler.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl std::fmt::Display for Version {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

/// Lifecycle state of a service node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum State {
    /// Not started yet.
    #[default]
    Pending,
    /// Dependencies are satisfied and the service can start.
    Ready,
    /// The service process is running.
    Running,
    /// The service exited successfully.
    Succeeded,
    /// The service exited with an error.
    Failed,
    /// A dependency cannot be satisfied.
    Blocked,
}

/// One node of the dependency graph.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Node {
    /// The service this node describes.
    pub service: ServiceSchema,
    /// Indices of the services this node depends on.
    pub deps: Vec<usize>,
    /// Indices of the services that depend on this node.
    pub dependents: Vec<usize>,
    /// Current lifecycle state.
    pub state: State,
}

/// Dependency graph over the configured services.
#[derive(Debug)]
pub struct Graph {
    /// One node per configured service, in configuration order.
    pub nodes: Vec<Node>,
    /// Maps a service name to its index in `nodes`.
    pub lookup: HashMap<String, usize>,
}
