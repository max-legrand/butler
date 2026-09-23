//! Service configuration loading and validation.

use crate::types::{CommandSpec, ServiceSchema};
use serde_yaml::Value;
use std::collections::HashSet;

/// Read a whole file into a string.
///
/// # Errors
///
/// Returns the operating system error message when `filepath` cannot be read.
pub fn try_read_file(filepath: &str) -> Result<String, String> {
    std::fs::read_to_string(filepath).map_err(|error| error.to_string())
}

/// Convert each YAML entry into a [`ServiceSchema`].
///
/// # Errors
///
/// Returns the message of the first entry that is not a valid service schema.
pub fn parse_service_schema_internal(yaml: Vec<Value>) -> Result<Vec<ServiceSchema>, String> {
    yaml.into_iter()
        .map(|entry| serde_yaml::from_value(entry).map_err(|error| error.to_string()))
        .collect()
}

/// Parse a service configuration file.
///
/// # Errors
///
/// Returns a message when the file cannot be read, when the contents are not
/// valid YAML, when the top level value is not a list, or when an entry is not
/// a valid service schema.
pub fn parse_service_schema(filepath: &str) -> Result<Vec<ServiceSchema>, String> {
    let contents = try_read_file(filepath)?;
    let yaml: Value = serde_yaml::from_str(&contents).map_err(|error| error.to_string())?;
    match yaml {
        Value::Sequence(services) => parse_service_schema_internal(services),
        _ => Err("Expected a list of services".to_string()),
    }
}

/// Check service names, command modes, and dependency references.
///
/// # Errors
///
/// Returns a message for the first duplicate name, invalid shell mode, or unknown dependency.
pub fn validate_service_schema(services: &[ServiceSchema]) -> Result<(), String> {
    let mut seen = HashSet::new();
    for service in services {
        if !seen.insert(service.name.as_str()) {
            return Err(format!("Duplicate service name: {}", service.name));
        }
        if service.run_as_shell && !matches!(service.command, CommandSpec::String(_)) {
            return Err(format!(
                "Service {} sets run-as-shell but command is not a string",
                service.name
            ));
        }
    }
    for service in services {
        for dependency in service.dependencies.as_deref().unwrap_or_default() {
            if !seen.contains(dependency.as_str()) {
                return Err(format!(
                    "Unknown dependency for service {}: {}",
                    service.name, dependency
                ));
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::validate_service_schema;
    use crate::types::{CommandSpec, ServiceSchema};

    #[test]
    fn command_accepts_string_and_sequence_with_shell_flag_defaulting_off() {
        let string_result = serde_yaml::from_str::<ServiceSchema>(
            r#"name: scalar
command: "echo hello"
"#,
        );
        assert!(string_result.is_ok());
        let Some(string_service) = string_result.ok() else {
            return;
        };
        assert_eq!(
            string_service.command,
            CommandSpec::String("echo hello".to_owned())
        );
        assert!(!string_service.run_as_shell);

        let sequence_result = serde_yaml::from_str::<ServiceSchema>(
            "name: argv\ncommand: [echo, hello]\nrun-as-shell: true\n",
        );
        assert!(sequence_result.is_ok());
        let Some(sequence_service) = sequence_result.ok() else {
            return;
        };
        assert_eq!(
            sequence_service.command,
            CommandSpec::Args(vec!["echo".to_owned(), "hello".to_owned()])
        );
        assert!(sequence_service.run_as_shell);
        assert!(validate_service_schema(&[sequence_service]).is_err());
    }
}
