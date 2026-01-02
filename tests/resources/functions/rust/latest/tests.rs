use openruntimes::{Context, Response, ResponseOptions};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::env;
use std::thread;
use std::time::Duration;

pub fn main(context: Context) -> Response {
    let action = context
        .req
        .headers
        .get("x-action")
        .map(|s| s.as_str())
        .unwrap_or("");

    match action {
        "plaintextResponse" => context.res.text("Hello World 👋", ResponseOptions::default()),

        "jsonResponse" => context.res.json(json!({
            "json": true,
            "message": "Developers are awesome."
        })),

        "customCharsetResponse" => {
            let mut headers = HashMap::new();
            headers.insert(
                "content-type".to_string(),
                "text/plain; charset=iso-8859-1".to_string(),
            );
            context.res.text("ÅÆ", context.res.with_headers(headers))
        }

        "uppercaseCharsetResponse" => {
            let mut headers = HashMap::new();
            headers.insert("content-type".to_string(), "TEXT/PLAIN".to_string());
            context.res.text("ÅÆ", context.res.with_headers(headers))
        }

        "multipartResponse" => {
            let body = r#"--12345
Content-Disposition: form-data; name="partOne"

Why just have one part?
--12345
Content-Disposition: form-data; name="partTwo"

When you can have two!
--12345--"#;
            let mut headers = HashMap::new();
            headers.insert(
                "content-type".to_string(),
                "multipart/form-data; boundary=12345".to_string(),
            );
            context.res.text(body, context.res.with_headers(headers))
        }

        "redirectResponse" => context.res.redirect(
            "https://github.com/",
            context.res.with_status_code(301),
        ),

        "emptyResponse" => context.res.empty(),

        "noResponse" => {
            context.res.text("This should be ignored, as it is not returned.", ResponseOptions::default());
            context.error("Return statement missing. return context.getRes().empty() if no response is expected.");
            context.res.text("", context.res.with_status_code(500))
        }

        "doubleResponse" => {
            context.res.text("This should be ignored.", ResponseOptions::default());
            context.res.text("This should be returned.", ResponseOptions::default())
        }

        "enforcedHeaders" => context.res.json(json!({
            "x-custom": context.req.headers.get("x-custom"),
            "x-custom-uppercase": context.req.headers.get("x-custom-uppercase"),
            "x-open-runtimes-custom": context.req.headers.get("x-open-runtimes-custom"),
        })),

        "headersResponse" => {
            let second_header = context
                .req
                .headers
                .get("x-open-runtimes-custom-in-header")
                .cloned()
                .unwrap_or_else(|| "missing".to_string());

            let cookie = context
                .req
                .headers
                .get("cookie")
                .cloned()
                .unwrap_or_else(|| "missing".to_string());

            let mut headers = HashMap::new();
            headers.insert("first-header".to_string(), "first-value".to_string());
            headers.insert("second-header".to_string(), second_header);
            headers.insert("cookie".to_string(), cookie);
            headers.insert(
                "x-open-runtimes-custom-out-header".to_string(),
                "third-value".to_string(),
            );

            context.res.text("OK", context.res.with_headers(headers))
        }

        "statusResponse" => context.res.text("FAIL", context.res.with_status_code(404)),

        "requestMethod" => context.res.text(&context.req.method, ResponseOptions::default()),

        "requestUrl" => context.res.json(json!({
            "url": context.req.url,
            "port": context.req.port,
            "path": context.req.path,
            "query": context.req.query,
            "queryString": context.req.query_string,
            "scheme": context.req.scheme,
            "host": context.req.host,
        })),

        "requestHeaders" => context.res.json(&context.req.headers),

        "requestBodyText" => context.res.text(context.req.body_text(), ResponseOptions::default()),

        "requestBodyJson" => {
            let mut ctx = context.clone();
            // Validate that body is not empty
            if ctx.req.body_binary().is_empty() {
                ctx.error("Cannot parse JSON body");
                return ctx.res.text("", ctx.res.with_status_code(500));
            }
            match ctx.req.body_json::<Value>() {
                Ok(body_json) => ctx.res.json(body_json),
                Err(_) => {
                    ctx.error("Cannot parse JSON body");
                    ctx.res.text("", ctx.res.with_status_code(500))
                }
            }
        }

        "requestBodyBinary" => context.res.binary(context.req.body_binary()),

        "requestBodyTextAuto" => context.res.text(context.req.body_text(), ResponseOptions::default()),

        "requestBodyJsonAuto" => {
            let mut ctx = context.clone();
            context.res.json(ctx.req.body())
        }

        "binaryResponse1" => {
            let bytes = vec![0, 10, 255];
            context.res.binary(bytes)
        }

        "binaryResponse2" => {
            let bytes = vec![0, 20, 255];
            context.res.binary(bytes)
        }

        "binaryResponse3" => {
            let bytes = vec![0, 30, 255];
            context.res.binary(bytes)
        }

        "binaryResponse4" => {
            let bytes = vec![0, 40, 255];
            context.res.binary(bytes)
        }

        "binaryResponse5" => {
            let bytes = vec![0, 50, 255];
            context.res.binary(bytes)
        }

        "envVars" => {
            let var = env::var("CUSTOM_ENV_VAR").unwrap_or_default();
            let empty_var = env::var("NOT_DEFINED_VAR").ok();

            context.res.json(json!({
                "var": var,
                "emptyVar": empty_var,
            }))
        }

        "logs" => {
            context.log("Native log");
            context.log("Debug log");
            context.error("Error log");

            context.log("Log+With+Plus+Symbol");

            context.log(42);
            context.log(4.2);
            context.log(true);

            context.log(json!({"objectKey": "objectValue"}));
            context.log(vec!["arrayValue"]);

            context.log("A".repeat(9000));
            context.error("B".repeat(9000));

            context.res.text("", ResponseOptions::default())
        }

        "library" => {
            let client = reqwest::blocking::Client::new();
            let todo_id = context.req.body_text();

            match client
                .get(format!("https://dummyjson.com/todos/{}", todo_id))
                .header("Accept", "application/json")
                .send()
            {
                Ok(response) => match response.json::<Value>() {
                    Ok(todo) => context.res.json(json!({
                        "todo": todo
                    })),
                    Err(e) => {
                        context.error(format!("JSON parse error: {:?}", e));
                        context.res.text("", context.res.with_status_code(500))
                    }
                },
                Err(e) => {
                    context.error(format!("HTTP request error: {:?}", e));
                    context.res.text("", context.res.with_status_code(500))
                }
            }
        }

        "timeout" => {
            context.log("Timeout start.");

            thread::sleep(Duration::from_secs(3));

            context.log("Timeout end.");

            context.res.text("Successful response.", ResponseOptions::default())
        }

        "deprecatedMethods" => {
            #[allow(deprecated)]
            let body = context.req.body_raw();
            #[allow(deprecated)]
            context.res.send(String::from_utf8_lossy(&body).to_string())
        }

        "deprecatedMethodsUntypedBody" => {
            #[allow(deprecated)]
            context.res.send("50".to_string())
        }

        "deprecatedMethodsBytesBody" => {
            use base64::{Engine as _, engine::general_purpose};
            let image = general_purpose::STANDARD.decode(
                "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAsSwAALEsBpT2WqQAAAMlQTFRFAAAA/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu+zZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/Tdt/TZv/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZv/TZv/TZu/TZu/TZu/TZu/TZu/TZu/Tdv/TZu/TZuuSxTMwAAAEN0Uk5TABN71PrYkxIu5P/jNyDf60XK3vkOWv11JiVsYazyawE8zInhtgd8bXTitQaw8WcBHMbPXP7pciMWIDLd1yIx5hWA/BXEE2wAAACqSURBVHicXY7PCwFxEMXfkxVKkVDbHrb2YMVBOZM/Xzk52AO1tYpNSdRGfhTzne8qvMM085mZ1yMAiky5wQxAWUZtmSmo8QkrhyeD63J5rxZ5ldvCAWzJoSNfPL+Axhb0jmiSCeCLE2MwSOFyraYdre0M3ko9u5c/EG4U9BL5Xpp2ECsQ04BcAEObjyNG6NvseV5/D1RC5mkl+niX4kuymXD+CzDlozT7gDdmIiQgwIp6VQAAAABJRU5ErkJggg=="
            ).unwrap();

            let mut headers = HashMap::new();
            headers.insert("content-type".to_string(), "image/png".to_string());

            openruntimes::Response {
                status_code: 200,
                body: image,
                headers,
            }
        }

        "binaryResponseLarge" => {
            let binary = context.req.body_binary();
            let digest = md5::compute(&binary);
            let hash = format!("{:x}", digest);

            let mut headers = HashMap::new();
            headers.insert("x-method".to_string(), context.req.method.clone());

            context.res.text(hash, context.res.with_headers(headers))
        }

        "spreadOperatorLogs" => {
            let engine = "open-runtimes";
            context.log_multiple(vec!["engine:".to_string(), engine.to_string()]);
            context.error_multiple(vec!["engine:".to_string(), engine.to_string()]);
            context.res.text("OK", ResponseOptions::default())
        }

        "setCookie" => {
            context.log("setCookie: Creating headers");
            let mut headers = HashMap::new();
            headers.insert("set-cookie".to_string(), vec![
                "cookie=value; path=/".to_string(),
                "cookie2=value2; path=/".to_string()
            ].join("\n"));
            headers.insert("some-header".to_string(), "some-value".to_string());

            context.log(format!("setCookie: Headers created: {:?}", headers));

            let response = context.res.text("OK", openruntimes::ResponseOptions {
                status_code: Some(200),
                headers: Some(headers),
            });

            context.log(format!("setCookie: Response created with status_code: {}", response.status_code));
            response
        }

        "setCookie2" => {
            let mut headers = HashMap::new();
            headers.insert("set-cookie".to_string(), vec![
                "cookie=value; path=/".to_string(),
                "cookie2=value2; path=/".to_string()
            ].join("\n"));
            headers.insert("some-header".to_string(), "some-value".to_string());

            context.res.text("OK", openruntimes::ResponseOptions {
                status_code: Some(200),
                headers: Some(headers),
            })
        }

        "errorTest" => {
            context.log("Before error...");
            panic!("Error!");
        }

        _ => {
            context.error("Unknown action in tests.rs");
            context.res.text("", context.res.with_status_code(500))
        }
    }
}
