//! Timestamped, colored logging to standard output.

use chrono::Local;

const RED: &str = "\x1b[0;31m";
const YELLOW: &str = "\x1b[0;33m";
const CYAN: &str = "\x1b[0;36m";
const PURPLE: &str = "\x1b[0;35m";
const RESET: &str = "\x1b[0m";

/// Severity of a log message, from most to least important.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Level {
    Error,
    Warn,
    Info,
    Debug,
}

impl Level {
    const fn label(self) -> &'static str {
        match self {
            Self::Error => "ERROR",
            Self::Warn => "WARN",
            Self::Info => "INFO",
            Self::Debug => "DEBUG",
        }
    }

    const fn color(self) -> &'static str {
        match self {
            Self::Error => RED,
            Self::Warn => YELLOW,
            Self::Info => CYAN,
            Self::Debug => PURPLE,
        }
    }
}

/// Writes messages at or above a configured level to standard output.
#[derive(Debug, Clone, Copy)]
pub struct Logger {
    level: Level,
}

fn parse_rgb(color: &str) -> Option<(u8, u8, u8)> {
    let hex = color.strip_prefix('#')?;
    if hex.len() != 6 {
        return None;
    }
    Some((
        u8::from_str_radix(hex.get(0..2)?, 16).ok()?,
        u8::from_str_radix(hex.get(2..4)?, 16).ok()?,
        u8::from_str_radix(hex.get(4..6)?, 16).ok()?,
    ))
}

impl Logger {
    /// Create a logger that drops messages below `level`.
    #[must_use]
    pub const fn new(level: Level) -> Self {
        Self { level }
    }

    /// Log an error message.
    pub fn error(&self, message: impl AsRef<str>) {
        self.log(Level::Error, message.as_ref());
    }

    /// Log a warning message.
    pub fn warn(&self, message: impl AsRef<str>) {
        self.log(Level::Warn, message.as_ref());
    }

    /// Log an informational message.
    pub fn info(&self, message: impl AsRef<str>) {
        self.log(Level::Info, message.as_ref());
    }

    /// Log a debug message.
    pub fn debug(&self, message: impl AsRef<str>) {
        self.log(Level::Debug, message.as_ref());
    }

    /// Log an informational message with `service` in place of the severity label.
    pub fn service(&self, service: impl AsRef<str>, color: Option<&str>, message: impl AsRef<str>) {
        if Level::Info > self.level {
            return;
        }
        let color = color.and_then(parse_rgb).map_or_else(
            || CYAN.to_owned(),
            |(red, green, blue)| format!("\x1b[38;2;{red};{green};{blue}m"),
        );
        let timestamp = Local::now().format("%Y-%m-%d %H:%M:%S");
        println!(
            "{color}{timestamp} [{}]:{RESET} {}",
            service.as_ref(),
            message.as_ref()
        );
    }

    fn log(self, level: Level, message: &str) {
        if level > self.level {
            return;
        }
        let timestamp = Local::now().format("%Y-%m-%d %H:%M:%S");
        println!(
            "{}{} [{}]:{RESET} {}",
            level.color(),
            timestamp,
            level.label(),
            message
        );
    }
}

#[cfg(test)]
mod tests {
    use super::parse_rgb;

    #[test]
    fn parses_rgb_service_color() {
        assert_eq!(parse_rgb("#74ACDF"), Some((116, 172, 223)));
        assert_eq!(parse_rgb("74ACDF"), None);
        assert_eq!(parse_rgb("#bad"), None);
    }
}
