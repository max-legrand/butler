use butler::app::{parse_service_schema, validate_service_schema};
use butler::graph::generate_graph;
use butler::logger::{Level, Logger};
use butler::types::Version;
use clap::Parser;

/// Butler service manager.
#[derive(Debug, Parser)]
#[command(name = "butler", about = "Butler service manager")]
struct Arguments {
    /// PATH path to the service config file
    #[arg(long, short)]
    file: Option<String>,
}

/// Version of Butler reported at startup.
const VERSION: Version = Version {
    major: 0,
    minor: 1,
    patch: 0,
};

fn main() {
    let arguments = Arguments::parse();
    let logger = Logger::new(Level::Info);
    logger.info(format!("butler v{VERSION}"));
    let config_file = arguments
        .file
        .unwrap_or_else(|| String::from("butler.yaml"));
    logger.info(format!("config file: {config_file}"));
    match parse_service_schema(&config_file) {
        Ok(services) => {
            logger.info(format!("loaded {} service(s)", services.len()));
            match validate_service_schema(&services) {
                Err(message) => logger.error(format!("config error: {message}")),
                Ok(()) => match generate_graph(&services) {
                    Ok(mut graph) => {
                        if let Err(message) = butler::runner::run(&mut graph, logger) {
                            logger.error(format!("runner error: {message}"));
                        }
                    }
                    Err(message) => logger.error(format!("graph error: {message}")),
                },
            }
        }
        Err(message) => logger.error(format!("config error: {message}")),
    }
}
