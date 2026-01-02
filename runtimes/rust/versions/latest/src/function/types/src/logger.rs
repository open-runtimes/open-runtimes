use serde_json::json;
use std::fs::{create_dir_all, OpenOptions};
use std::io::Write as IoWrite;
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};

const MAX_LOG_SIZE: usize = 8000;

#[derive(Clone)]
pub enum LoggerType {
    Log,
    Error,
}

#[derive(Clone)]
pub struct Logger {
    pub id: String,
    enabled: bool,
    include_native: bool,
    logs: Arc<Mutex<Vec<serde_json::Value>>>,
}

impl Logger {
    pub fn new(logging: &str, log_id: Option<String>) -> Result<Self, String> {
        let enabled = logging == "" || logging == "enabled";
        let include_native = logging == "enabled" || logging == "disabled";

        let id = if let Some(provided_id) = log_id {
            provided_id
        } else if std::env::var("OPEN_RUNTIMES_ENV").unwrap_or_default() == "development" {
            "dev".to_string()
        } else {
            Self::generate_id()
        };

        Ok(Logger {
            id,
            enabled,
            include_native,
            logs: Arc::new(Mutex::new(Vec::new())),
        })
    }

    fn generate_id() -> String {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap();
        let sec = now.as_secs();
        let msec = now.subsec_millis();

        let sec_hex = format!("{:x}", sec);
        let msec_hex = format!("{:05x}", msec);

        let mut random_padding = String::new();
        for _ in 0..7 {
            let rand_digit = (SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .unwrap()
                .subsec_nanos() % 16) as u8;
            random_padding.push_str(&format!("{:x}", rand_digit));
        }

        format!("{}{}{}", sec_hex, msec_hex, random_padding)
    }

    pub fn write(&self, messages: Vec<String>, log_type: LoggerType, native: bool) {
        if !native && !self.enabled {
            return;
        }

        if native && !self.include_native {
            return;
        }

        let type_str = match log_type {
            LoggerType::Log => "log",
            LoggerType::Error => "error",
        };

        let stream = match log_type {
            LoggerType::Log => "stdout",
            LoggerType::Error => "stderr",
        };

        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis();

        let mut message = messages.join(" ");

        if message.len() > MAX_LOG_SIZE {
            message.truncate(MAX_LOG_SIZE);
            message.push_str("... Log truncated due to size limit (8000 characters)");
        }

        let log_entry = json!({
            "timestamp": timestamp,
            "type": type_str,
            "message": message,
            "stream": stream,
        });

        if let Ok(mut logs) = self.logs.lock() {
            logs.push(log_entry);
        }

        if native {
            let message = messages.join(" ");
            match log_type {
                LoggerType::Log => println!("{}", message),
                LoggerType::Error => eprintln!("{}", message),
            }
        }
    }

    pub fn override_native_logs(&mut self) {
        // In Rust, capturing stdout/stderr is complex and not typically done
        // We'll handle native logs through our write method instead
    }

    pub fn revert_native_logs(&mut self) {
        // Matching the override method
    }

    pub fn end(&self) {
        if !self.enabled {
            return;
        }

        let logs_dir = "/mnt/logs";
        if let Err(_) = create_dir_all(logs_dir) {
            eprintln!("Failed to create logs directory");
            return;
        }

        let logs_file_path = format!("{}/{}_logs.log", logs_dir, self.id);
        let errors_file_path = format!("{}/{}_errors.log", logs_dir, self.id);

        let mut logs_file = match OpenOptions::new()
            .create(true)
            .write(true)
            .append(true)
            .open(&logs_file_path)
        {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Failed to open logs file: {}", e);
                return;
            }
        };

        let mut errors_file = match OpenOptions::new()
            .create(true)
            .write(true)
            .append(true)
            .open(&errors_file_path)
        {
            Ok(f) => f,
            Err(e) => {
                eprintln!("Failed to open errors file: {}", e);
                return;
            }
        };

        if let Ok(logs) = self.logs.lock() {
            for log in logs.iter() {
                let log_type = log.get("type").and_then(|v| v.as_str()).unwrap_or("");

                if let Ok(log_str) = serde_json::to_string(log) {
                    let file_to_write = if log_type == "error" {
                        &mut errors_file
                    } else {
                        &mut logs_file
                    };

                    if let Err(e) = writeln!(file_to_write, "{}", log_str) {
                        eprintln!("Failed to write log: {}", e);
                    }
                }
            }
        }

        if let Err(e) = logs_file.flush() {
            eprintln!("Failed to flush logs file: {}", e);
        }

        if let Err(e) = errors_file.flush() {
            eprintln!("Failed to flush errors file: {}", e);
        }
    }
}

pub fn format_log_message(value: &dyn std::fmt::Debug) -> String {
    format!("{:?}", value)
}
