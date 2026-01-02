use std::fs::{create_dir_all, OpenOptions};
use std::io::Write as IoWrite;
use std::sync::{Arc, Mutex};
use std::time::{SystemTime, UNIX_EPOCH};
use rand::Rng;

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
    logs: Arc<Mutex<Vec<String>>>,
    errors: Arc<Mutex<Vec<String>>>,
}

impl Logger {
    pub fn new(logging: &str, log_id: Option<String>) -> Result<Self, String> {
        let enabled = logging == "enabled";
        let include_native = logging == "enabled" || logging == "disabled";

        let id = if enabled {
            log_id.unwrap_or_else(|| {
                // Check if we're in development mode
                match std::env::var("OPEN_RUNTIMES_ENV") {
                    Ok(env) if env == "development" => "dev".to_string(),
                    _ => generate_id()
                }
            })
        } else {
            String::new()
        };

        Ok(Logger {
            id,
            enabled,
            include_native,
            logs: Arc::new(Mutex::new(Vec::new())),
            errors: Arc::new(Mutex::new(Vec::new())),
        })
    }

    pub fn write(&self, messages: Vec<String>, log_type: LoggerType, native: bool) {
        if !native && !self.enabled {
            return;
        }

        if native && !self.include_native {
            return;
        }

        // Join messages into a single string
        let mut message = messages.join(" ");

        // Truncate at 8000 characters like other runtimes
        if message.len() > 8000 {
            message.truncate(8000);
            message.push_str("... Log truncated due to size limit (8000 characters)");
        }

        // Store in appropriate vector
        match log_type {
            LoggerType::Log => {
                if let Ok(mut logs) = self.logs.lock() {
                    logs.push(message.clone());
                }
            }
            LoggerType::Error => {
                if let Ok(mut errors) = self.errors.lock() {
                    errors.push(message.clone());
                }
            }
        }

        if native {
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
            // Silently ignore directory creation failures
            return;
        }

        // Write logs file
        let logs_path = format!("{}/{}_logs.log", logs_dir, self.id);
        if let Ok(logs) = self.logs.lock() {
            if let Ok(mut file) = OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open(&logs_path)
            {
                for log in logs.iter() {
                    let _ = writeln!(file, "{}", log);
                }
            }
        }

        // Write errors file
        let errors_path = format!("{}/{}_errors.log", logs_dir, self.id);
        if let Ok(errors) = self.errors.lock() {
            if let Ok(mut file) = OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open(&errors_path)
            {
                for error in errors.iter() {
                    let _ = writeln!(file, "{}", error);
                }
            }
        }
    }
}

pub fn format_log_message(value: &dyn std::fmt::Debug) -> String {
    format!("{:?}", value)
}

// Recreated from https://www.php.net/manual/en/function.uniqid.php
fn generate_id() -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap();
    let sec = now.as_secs();
    let msec = now.subsec_millis();

    // Convert to hex: sec (8 chars) + msec padded to 5 hex chars
    let base_id = format!("{:x}{:05x}", sec, msec);

    // Add 7 random hex digits
    let mut rng = rand::thread_rng();
    let random_padding: String = (0..7)
        .map(|_| format!("{:x}", rng.gen_range(0..16)))
        .collect();

    format!("{}{}", base_id, random_padding)
}
