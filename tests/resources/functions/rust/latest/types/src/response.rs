use serde::Serialize;
use serde_json;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct Response {
    pub status_code: u16,
    pub body: Vec<u8>,
    pub headers: HashMap<String, String>,
}

impl Response {
    pub fn new() -> Self {
        Response {
            status_code: 200,
            body: Vec::new(),
            headers: HashMap::new(),
        }
    }
}

impl Default for Response {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug, Default)]
pub struct ResponseOptions {
    pub status_code: Option<u16>,
    pub headers: Option<HashMap<String, String>>,
}

impl ResponseOptions {
    pub fn new() -> Self {
        ResponseOptions {
            status_code: None,
            headers: None,
        }
    }
}

#[derive(Clone)]
pub struct ContextResponse;

impl ContextResponse {
    pub fn new() -> Self {
        ContextResponse
    }

    pub fn text<S: Into<String>>(&self, text: S, options: ResponseOptions) -> Response {
        let text_string = text.into();
        let mut response = Response {
            status_code: options.status_code.unwrap_or(200),
            body: text_string.into_bytes(),
            headers: options.headers.unwrap_or_default(),
        };

        if !response.headers.contains_key("content-type") {
            response
                .headers
                .insert("content-type".to_string(), "text/plain".to_string());
        }

        response
    }

    pub fn json<T: Serialize>(&self, data: T) -> Response {
        let json_string = serde_json::to_string(&data).unwrap_or_else(|_| "{}".to_string());
        let mut response = Response {
            status_code: 200,
            body: json_string.into_bytes(),
            headers: HashMap::new(),
        };

        response
            .headers
            .insert("content-type".to_string(), "application/json".to_string());

        response
    }

    pub fn binary(&self, data: Vec<u8>) -> Response {
        let mut response = Response {
            status_code: 200,
            body: data,
            headers: HashMap::new(),
        };

        response.headers.insert(
            "content-type".to_string(),
            "application/octet-stream".to_string(),
        );

        response
    }

    pub fn empty(&self) -> Response {
        Response {
            status_code: 204,
            body: Vec::new(),
            headers: HashMap::new(),
        }
    }

    pub fn redirect<S: Into<String>>(&self, url: S, options: ResponseOptions) -> Response {
        let url_string = url.into();
        let mut headers = options.headers.unwrap_or_default();
        headers.insert("location".to_string(), url_string);

        Response {
            status_code: options.status_code.unwrap_or(301),
            body: Vec::new(),
            headers,
        }
    }

    #[deprecated(note = "Use text(), json(), or binary() instead")]
    pub fn send<S: Into<String>>(&self, data: S) -> Response {
        self.text(data, ResponseOptions::default())
    }

    pub fn with_status_code(&self, status_code: u16) -> ResponseOptions {
        ResponseOptions {
            status_code: Some(status_code),
            headers: None,
        }
    }

    pub fn with_headers(&self, headers: HashMap<String, String>) -> ResponseOptions {
        ResponseOptions {
            status_code: None,
            headers: Some(headers),
        }
    }
}

impl Default for ContextResponse {
    fn default() -> Self {
        Self::new()
    }
}
