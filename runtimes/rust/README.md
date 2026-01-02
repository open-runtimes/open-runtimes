# Rust Runtime
The runtime itself uses [Tokio](https://tokio.rs/) and [Hyper](https://hyper.rs/) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `main.rs` file:

```bash
mkdir rust-function && cd rust-function
tee -a main.rs << 'END'
use openruntimes::{Context, Response, ResponseOptions};
use serde_json::json;

pub fn main(context: Context) -> Response {
    context.res.json(json!({
        "message": "Hello from Rust!",
        "random": rand::random::<f64>()
    }))
}
END
```

2. Add `Cargo.toml` file to mark function as an open runtimes handler:

```bash
tee -a Cargo.toml << 'END'
[package]
name = "handler"
version = "0.1.0"
edition = "2021"

[lib]
name = "handler"
path = "main.rs"

[dependencies]
openruntimes = { path = "/mnt/code/types-for-rust" }
serde_json = "1.0"
rand = "0.8"
END
```

3. Build the code:
> Examples use rust-1.83, but you can use any from `versions` directory.

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=main.rs --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/rust:v5-1.83 sh helpers/build.sh
```

4. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/rust:v5-1.83 sh helpers/start.sh
```

5. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"message":"Hello from Rust!","random":0.7232589496628183}` with random float will be displayed after the execution.

## Notes

- When writing function for this runtime, ensure it is named `main`. An example of this is:

```rust
use openruntimes::{Context, Response};

pub fn main(context: Context) -> Response {
    context.res.text("Hello, World!", Default::default())
}
```

- Ensure your Cargo.toml defines a library with the name `handler`:

```toml
[lib]
name = "handler"
path = "main.rs"  # or your entry file
```

- To handle dependencies, add them to `Cargo.toml` file. Dependencies will be automatically installed during the build process.

- The default entrypoint is `main.rs`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.rs`.

- **Logging:** Always use `context.log()` and `context.error()` for logging. Native Rust logging (`println!`, `eprintln!`, `dbg!`) is **not captured** by the runtime. See the [Logging](#logging) section for details.

## Context API

The `Context` object provides access to request data and response helpers:

### Request (`context.req`)

```rust
// Access headers
let user_agent = context.req.headers.get("user-agent");

// Get request method
let method = &context.req.method;

// Parse JSON body
let body: MyStruct = context.req.body_json()?;

// Get text body
let text = context.req.body_text();

// Get binary body
let bytes = context.req.body_binary();

// Access URL components
let path = &context.req.path;
let query = &context.req.query;
let url = &context.req.url;
```

### Response (`context.res`)

```rust
// Return JSON
context.res.json(json!({
    "message": "Hello"
}))

// Return text
context.res.text("Hello, World!", ResponseOptions::default())

// Return binary
context.res.binary(vec![0, 1, 2, 3])

// Return empty response
context.res.empty()

// Redirect
context.res.redirect("https://example.com/", context.res.with_status_code(301))

// Custom status code
context.res.text("Not Found", context.res.with_status_code(404))

// Custom headers
use std::collections::HashMap;
let mut headers = HashMap::new();
headers.insert("X-Custom-Header".to_string(), "value".to_string());
context.res.text("Hello", context.res.with_headers(headers))
```

### Logging

> **Important:** Always use `context.log()` and `context.error()` for logging. 
> Native Rust logging methods like `println!()`, `eprintln!()`, `print!()`, or `dbg!()` are **not captured** by the runtime and will not appear in execution logs.

```rust
// Log messages
context.log("Debug message");
context.log(42);
context.log(json!({"key": "value"}));

// Log errors
context.error("Something went wrong");

// Multiple arguments
context.log_multiple(vec!["First".to_string(), "Second".to_string()]);
```

**Do NOT use:**
```rust
// ❌ These will NOT be captured in logs
println!("This won't appear in logs");
eprintln!("This won't appear either");
dbg!(some_variable);
```

**Use instead:**
```rust
// ✅ These WILL be captured in logs
context.log("This will appear in logs");
context.error("This error will appear in logs");
```

## Environment Variables

Access environment variables using the standard library:

```rust
use std::env;

let custom_var = env::var("CUSTOM_ENV_VAR").unwrap_or_default();
```

## External HTTP Requests

Use the `reqwest` crate for HTTP requests:

```rust
use reqwest;
use serde_json::Value;

pub fn main(context: Context) -> Response {
    let client = reqwest::blocking::Client::new();

    match client.get("https://api.example.com/data")
        .send()
        .and_then(|r| r.json::<Value>())
    {
        Ok(data) => context.res.json(data),
        Err(e) => {
            context.error(format!("Request failed: {:?}", e));
            context.res.text("", context.res.with_status_code(500))
        }
    }
}
```

Don't forget to add `reqwest` to your `Cargo.toml`:

```toml
[dependencies]
reqwest = { version = "0.12", features = ["blocking", "json"] }
```

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
