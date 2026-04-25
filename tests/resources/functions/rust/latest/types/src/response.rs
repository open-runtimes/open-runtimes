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

#[derive(Clone)]
pub struct ContextResponse;

impl ContextResponse {
    pub fn new() -> Self {
        ContextResponse
    }

    pub fn text<S: Into<String>>(
        &self,
        text: S,
        status_code: Option<u16>,
        headers: Option<HashMap<String, String>>,
    ) -> Response {
        let text_string = text.into();
        let mut response = Response {
            status_code: status_code.unwrap_or(200),
            body: text_string.into_bytes(),
            headers: headers.unwrap_or_default(),
        };

        if !response.headers.contains_key("content-type") {
            response
                .headers
                .insert("content-type".to_string(), "text/plain".to_string());
        }

        response
    }

    pub fn json<T: Serialize>(
        &self,
        data: T,
        status_code: Option<u16>,
        headers: Option<HashMap<String, String>>,
    ) -> Response {
        let json_string = serde_json::to_string(&data).unwrap_or_else(|_| "{}".to_string());
        let mut response = Response {
            status_code: status_code.unwrap_or(200),
            body: json_string.into_bytes(),
            headers: headers.unwrap_or_default(),
        };

        if !response.headers.contains_key("content-type") {
            response
                .headers
                .insert("content-type".to_string(), "application/json".to_string());
        }

        response
    }

    pub fn binary(
        &self,
        data: Vec<u8>,
        status_code: Option<u16>,
        headers: Option<HashMap<String, String>>,
    ) -> Response {
        let mut response = Response {
            status_code: status_code.unwrap_or(200),
            body: data,
            headers: headers.unwrap_or_default(),
        };

        if !response.headers.contains_key("content-type") {
            response.headers.insert(
                "content-type".to_string(),
                "application/octet-stream".to_string(),
            );
        }

        response
    }

    pub fn empty(&self) -> Response {
        Response {
            status_code: 204,
            body: Vec::new(),
            headers: HashMap::new(),
        }
    }

    pub fn redirect<S: Into<String>>(
        &self,
        url: S,
        status_code: Option<u16>,
        headers: Option<HashMap<String, String>>,
    ) -> Response {
        let url_string = url.into();
        let mut response_headers = headers.unwrap_or_default();
        response_headers.insert("location".to_string(), url_string);

        Response {
            status_code: status_code.unwrap_or(301),
            body: Vec::new(),
            headers: response_headers,
        }
    }

    #[deprecated(note = "Use text(), json(), or binary() instead")]
    pub fn send<S: Into<String>>(&self, data: S) -> Response {
        self.text(data, None, None)
    }
}

impl Default for ContextResponse {
    fn default() -> Self {
        Self::new()
    }
}
