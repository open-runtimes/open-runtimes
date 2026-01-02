use crate::logger::{Logger, LoggerType};
use crate::request::ContextRequest;
use crate::response::ContextResponse;

pub trait LogMessage {
    fn to_log_string(&self) -> String;
}

// Implement for basic types
impl LogMessage for &str {
    fn to_log_string(&self) -> String {
        self.to_string()
    }
}

impl LogMessage for String {
    fn to_log_string(&self) -> String {
        self.clone()
    }
}

impl LogMessage for i32 {
    fn to_log_string(&self) -> String {
        self.to_string()
    }
}

impl LogMessage for i64 {
    fn to_log_string(&self) -> String {
        self.to_string()
    }
}

impl LogMessage for u32 {
    fn to_log_string(&self) -> String {
        self.to_string()
    }
}

impl LogMessage for u64 {
    fn to_log_string(&self) -> String {
        self.to_string()
    }
}

impl LogMessage for f32 {
    fn to_log_string(&self) -> String {
        self.to_string()
    }
}

impl LogMessage for f64 {
    fn to_log_string(&self) -> String {
        self.to_string()
    }
}

impl LogMessage for bool {
    fn to_log_string(&self) -> String {
        self.to_string()
    }
}

// Implement for JSON types
impl LogMessage for serde_json::Value {
    fn to_log_string(&self) -> String {
        serde_json::to_string(self).unwrap_or_else(|_| format!("{:?}", self))
    }
}

impl LogMessage for Vec<&str> {
    fn to_log_string(&self) -> String {
        serde_json::to_string(self).unwrap_or_else(|_| format!("{:?}", self))
    }
}

impl LogMessage for Vec<String> {
    fn to_log_string(&self) -> String {
        serde_json::to_string(self).unwrap_or_else(|_| format!("{:?}", self))
    }
}

#[derive(Clone)]
pub struct Context {
    pub req: ContextRequest,
    pub res: ContextResponse,
    logger: Logger,
}

impl Context {
    pub fn new(logger: Logger) -> Self {
        Context {
            req: ContextRequest::new(),
            res: ContextResponse::new(),
            logger,
        }
    }

    pub fn log<T: LogMessage>(&self, message: T) {
        let msg = message.to_log_string();
        self.logger.write(vec![msg], LoggerType::Log, false);
    }

    pub fn log_multiple(&self, messages: Vec<String>) {
        self.logger.write(messages, LoggerType::Log, false);
    }

    pub fn error<T: LogMessage>(&self, message: T) {
        let msg = message.to_log_string();
        self.logger.write(vec![msg], LoggerType::Error, false);
    }

    pub fn error_multiple(&self, messages: Vec<String>) {
        self.logger.write(messages, LoggerType::Error, false);
    }

    pub fn get_logger(&self) -> &Logger {
        &self.logger
    }

    pub fn get_logger_mut(&mut self) -> &mut Logger {
        &mut self.logger
    }
}
