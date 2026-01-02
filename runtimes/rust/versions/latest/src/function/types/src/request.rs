use serde::Deserialize;
use serde_json;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct ContextRequest {
    pub headers: HashMap<String, String>,
    pub method: String,
    pub url: String,
    pub scheme: String,
    pub host: String,
    pub port: u16,
    pub path: String,
    pub query_string: String,
    pub query: HashMap<String, String>,
    body_binary: Vec<u8>,
    body_parsed: Option<serde_json::Value>,
}

impl ContextRequest {
    pub fn new() -> Self {
        ContextRequest {
            headers: HashMap::new(),
            method: String::new(),
            url: String::new(),
            scheme: String::new(),
            host: String::new(),
            port: 80,
            path: String::new(),
            query_string: String::new(),
            query: HashMap::new(),
            body_binary: Vec::new(),
            body_parsed: None,
        }
    }

    pub fn set_body_binary(&mut self, data: Vec<u8>) {
        self.body_binary = data;
        self.body_parsed = None;
    }

    pub fn body_binary(&self) -> Vec<u8> {
        self.body_binary.clone()
    }

    pub fn body_text(&self) -> String {
        String::from_utf8_lossy(&self.body_binary).to_string()
    }

    pub fn body_json<T>(&mut self) -> Result<T, serde_json::Error>
    where
        T: for<'de> Deserialize<'de>,
    {
        if self.body_parsed.is_none() {
            let value: serde_json::Value = serde_json::from_slice(&self.body_binary)?;
            self.body_parsed = Some(value);
        }

        if let Some(ref parsed) = self.body_parsed {
            serde_json::from_value(parsed.clone())
        } else {
            serde_json::from_slice(&self.body_binary)
        }
    }

    pub fn body(&mut self) -> serde_json::Value {
        let content_type = self
            .headers
            .get("content-type")
            .map(|s| s.as_str())
            .unwrap_or("");

        if content_type.contains("application/json") {
            // Return empty object for empty body with JSON content-type
            if self.body_binary.is_empty() {
                return serde_json::Value::Object(serde_json::Map::new());
            }

            if self.body_parsed.is_none() {
                if let Ok(value) = serde_json::from_slice(&self.body_binary) {
                    self.body_parsed = Some(value);
                }
            }
            self.body_parsed
                .clone()
                .unwrap_or(serde_json::Value::Object(serde_json::Map::new()))
        } else {
            serde_json::Value::String(self.body_text())
        }
    }

    #[deprecated(note = "Use body_binary() instead")]
    pub fn body_raw(&self) -> Vec<u8> {
        self.body_binary()
    }
}

impl Default for ContextRequest {
    fn default() -> Self {
        Self::new()
    }
}
