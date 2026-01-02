use http_body_util::{BodyExt, Full};
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response, StatusCode};
use hyper_util::rt::TokioIo;
use openruntimes::{Context, Logger, LoggerType};
use serde_json;
use std::collections::HashMap;
use std::env;
use std::net::SocketAddr;
use std::time::Duration;
use tokio::net::TcpListener;
use tokio::time::timeout;

async fn handle_request(
    req: Request<hyper::body::Incoming>,
) -> Result<Response<Full<Bytes>>, hyper::Error> {
    let path = req.uri().path();

    // Health check endpoint
    if path == "/__opr/health" {
        return Ok(Response::builder()
            .status(StatusCode::OK)
            .header("content-type", "text/plain")
            .body(Full::new(Bytes::from("OK")))
            .unwrap());
    }

    // Telemetry endpoint
    if path == "/__opr/timings" {
        match tokio::fs::read_to_string("/mnt/telemetry/timings.txt").await {
            Ok(timings) => {
                return Ok(Response::builder()
                    .status(StatusCode::OK)
                    .header("content-type", "text/plain; charset=utf-8")
                    .body(Full::new(Bytes::from(timings)))
                    .unwrap());
            }
            Err(_) => {
                return Ok(Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .body(Full::new(Bytes::new()))
                    .unwrap());
            }
        }
    }

    // Get logging settings
    let logging = req
        .headers()
        .get("x-open-runtimes-logging")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    let log_id = req
        .headers()
        .get("x-open-runtimes-log-id")
        .and_then(|v| v.to_str().ok())
        .map(|s| s.to_string());

    let mut logger = match Logger::new(logging, log_id) {
        Ok(l) => l,
        Err(e) => {
            let error_logger = Logger::new("enabled", None).unwrap();
            error_logger.write(vec![e.clone()], LoggerType::Error, false);
            error_logger.end();

            return Ok(Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .header("x-open-runtimes-log-id", error_logger.id.as_str())
                .header("content-type", "text/plain")
                .body(Full::new(Bytes::new()))
                .unwrap());
        }
    };

    match action(req, &mut logger).await {
        Ok(response) => Ok(response),
        Err(e) => {
            logger.write(vec![e], LoggerType::Error, false);
            logger.end();

            Ok(Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .header("x-open-runtimes-log-id", logger.id.as_str())
                .header("content-type", "text/plain")
                .body(Full::new(Bytes::new()))
                .unwrap())
        }
    }
}

fn execute_user_function(ctx: Context, log: Logger) -> openruntimes::Response {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| handler::main(ctx.clone()))) {
        Ok(response) => response,
        Err(panic_info) => {
            let panic_msg = if let Some(s) = panic_info.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = panic_info.downcast_ref::<String>() {
                s.clone()
            } else {
                "Unknown panic".to_string()
            };
            log.write(vec![panic_msg], openruntimes::LoggerType::Error, false);
            ctx.res.text("", ctx.res.with_status_code(500))
        }
    }
}

async fn action(
    req: Request<hyper::body::Incoming>,
    logger: &mut Logger,
) -> Result<Response<Full<Bytes>>, String> {
    // Parse logging settings
    let logging = req
        .headers()
        .get("x-open-runtimes-logging")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("")
        .to_string();

    // Parse timeout
    let timeout_header = req
        .headers()
        .get("x-open-runtimes-timeout")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    let safe_timeout: Option<u64> = if !timeout_header.is_empty() {
        match timeout_header.parse::<u64>() {
            Ok(t) if t > 0 => Some(t),
            _ => {
                return Ok(Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .header("content-type", "text/plain")
                    .body(Full::new(Bytes::from(
                        "Header \"x-open-runtimes-timeout\" must be an integer greater than 0.",
                    )))
                    .unwrap());
            }
        }
    } else {
        None
    };

    // Verify secret
    let secret = req
        .headers()
        .get("x-open-runtimes-secret")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    let server_secret = env::var("OPEN_RUNTIMES_SECRET").unwrap_or_default();

    if !server_secret.is_empty() && secret != server_secret {
        return Ok(Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .header("content-type", "text/plain")
            .body(Full::new(Bytes::from(
                "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.",
            )))
            .unwrap());
    }

    // Parse headers
    let mut headers: HashMap<String, String> = HashMap::new();
    for (key, value) in req.headers() {
        let key_lower = key.as_str().to_lowercase();

        if !key_lower.starts_with("x-open-runtimes-") {
            let mut value_str = value.to_str().unwrap_or("").to_string();

            if key_lower == "content-type" {
                value_str = value_str.to_lowercase();
            }

            headers.insert(key_lower, value_str);
        }
    }

    // Enforce headers from environment
    let headers_env = env::var("OPEN_RUNTIMES_HEADERS").unwrap_or_else(|_| "{}".to_string());
    if let Ok(enforced_headers) = serde_json::from_str::<HashMap<String, serde_json::Value>>(&headers_env) {
        for (key, value) in enforced_headers {
            let value_string = match value {
                serde_json::Value::String(s) => s,
                serde_json::Value::Number(n) => n.to_string(),
                serde_json::Value::Bool(b) => b.to_string(),
                serde_json::Value::Null => String::new(),
                _ => value.to_string(),
            };
            headers.insert(key.to_lowercase(), value_string);
        }
    }

    // Parse request details
    let method = req.method().to_string();

    let scheme_header = req
        .headers()
        .get("x-forwarded-proto")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("http");
    let scheme = scheme_header.to_string();

    let default_port = if scheme == "https" { 443 } else { 80 };

    let host_header = req
        .headers()
        .get("host")
        .and_then(|v| v.to_str().ok())
        .unwrap_or("");

    let (host, port) = if host_header.contains(':') {
        let parts: Vec<&str> = host_header.splitn(2, ':').collect();
        let h = parts[0].to_string();
        let p = parts.get(1).and_then(|s| s.parse::<u16>().ok()).unwrap_or(default_port);
        (h, p)
    } else {
        (host_header.to_string(), default_port)
    };

    let path = req.uri().path().to_string();
    let query_string = req.uri().query().unwrap_or("").to_string();

    // Parse query parameters
    let mut query: HashMap<String, String> = HashMap::new();
    if !query_string.is_empty() {
        for pair in query_string.split('&') {
            if let Some((key, value)) = pair.split_once('=') {
                query.insert(
                    key.to_string(),
                    urlencoding::decode(value).unwrap_or_default().to_string(),
                );
            } else {
                query.insert(pair.to_string(), String::new());
            }
        }
    }

    // Build URL
    let port_in_url = if port != default_port {
        format!(":{}", port)
    } else {
        String::new()
    };

    let query_string_in_url = if !query_string.is_empty() {
        format!("?{}", query_string)
    } else {
        String::new()
    };

    let request_url = format!("{}://{}{}{}{}", scheme, host, port_in_url, path, query_string_in_url);

    // Read body with 20MB size limit (20 * 1024 * 1024 = 20971520 bytes)
    const MAX_BODY_SIZE: usize = 20 * 1024 * 1024;

    let body_bytes = match req.collect().await {
        Ok(collected) => {
            let bytes = collected.to_bytes().to_vec();
            if bytes.len() > MAX_BODY_SIZE {
                return Ok(Response::builder()
                    .status(StatusCode::INTERNAL_SERVER_ERROR)
                    .header("content-type", "text/plain")
                    .body(Full::new(Bytes::from(
                        "Request body size exceeds 20MB limit.",
                    )))
                    .unwrap());
            }
            bytes
        }
        Err(_) => {
            return Err("Could not parse body into a string.".to_string());
        }
    };

    // Create context
    let mut context = Context::new(logger.clone());
    context.req.headers = headers;
    context.req.method = method;
    context.req.url = request_url;
    context.req.scheme = scheme;
    context.req.host = host;
    context.req.port = port;
    context.req.path = path;
    context.req.query_string = query_string;
    context.req.query = query;
    context.req.set_body_binary(body_bytes);

    // Execute user function with timeout
    let output = if let Some(timeout_secs) = safe_timeout {
        let ctx = context.clone();
        let log = logger.clone();
        let user_function = tokio::task::spawn_blocking(move || {
            execute_user_function(ctx, log)
        });

        match timeout(Duration::from_secs(timeout_secs), user_function).await {
            Ok(Ok(result)) => result,
            Ok(Err(_)) => {
                context.error("Task join error.");
                context.res.text("", context.res.with_status_code(500))
            }
            Err(_) => {
                context.error("Execution timed out.");
                context.res.text("", context.res.with_status_code(500))
            }
        }
    } else {
        let ctx = context.clone();
        let log = logger.clone();
        let handle = tokio::task::spawn_blocking(move || {
            execute_user_function(ctx, log)
        });

        match handle.await {
            Ok(result) => result,
            Err(_) => {
                context.error("Task join error.");
                context.res.text("", context.res.with_status_code(500))
            }
        }
    };

    logger.end();

    // Process output
    let status_code = if output.status_code == 0 {
        200
    } else {
        output.status_code
    };

    let mut output_headers = HashMap::new();
    for (key, value) in output.headers {
        let key_lower = key.to_lowercase();
        if !key_lower.starts_with("x-open-runtimes-") {
            output_headers.insert(key_lower, value);
        }
    }

    // Set content-type with charset
    let mut content_type = output_headers
        .get("content-type")
        .cloned()
        .unwrap_or_else(|| "text/plain".to_string())
        .to_lowercase();

    if !content_type.starts_with("multipart/") && !content_type.contains("charset=") {
        content_type = format!("{}; charset=utf-8", content_type);
    }

    output_headers.insert("content-type".to_string(), content_type);

    // Build response
    let mut response_builder = Response::builder().status(status_code);

    let log_id_value = if logging == "disabled" { "" } else { logger.id.as_str() };
    response_builder = response_builder.header("x-open-runtimes-log-id", log_id_value);

    for (key, value) in output_headers {
        if key == "set-cookie" {
            for cookie_value in value.split('\n') {
                response_builder = response_builder.header(&key, cookie_value);
            }
        } else {
            response_builder = response_builder.header(key, value);
        }
    }

    Ok(response_builder.body(Full::new(Bytes::from(output.body))).unwrap())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let addr = SocketAddr::from(([0, 0, 0, 0], 3000));
    let listener = TcpListener::bind(addr).await?;

    println!("HTTP server successfully started!");

    loop {
        let (stream, _) = listener.accept().await?;
        let io = TokioIo::new(stream);

        tokio::task::spawn(async move {
            if let Err(err) = http1::Builder::new()
                .serve_connection(io, service_fn(handle_request))
                .await
            {
                eprintln!("Error serving connection: {:?}", err);
            }
        });
    }
}
