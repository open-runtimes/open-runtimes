---
name: new-runtime
description: Add an entirely new function runtime to open-runtimes. Generates all required files (server, Dockerfile, helpers, tests, CI config) so the runtime passes the test suite in one shot.
disable-model-invocation: true
argument-hint: {language} {version}
allowed-tools: Read Write Edit Glob Grep Bash Agent WebSearch WebFetch
---

# New Runtime Generator

Generate a complete, test-passing runtime for the open-runtimes project.

## Arguments

The user provides: `{language} {version}`

- `language` - The programming language name (e.g. `rust`, `zig`, `elixir`, `lua`, `perl`, `r`, `haskell`, `scala`, `ocaml`)
- `version` - The initial version to support (e.g. `1.80`, `3.12`, `0.13`)

## Process

### Phase 1: Research

Before writing any code, gather the information needed to make correct decisions. Run these research steps **in parallel** using subagents:

1. **Docker image**: Find the official alpine-based Docker image for the language + version on Docker Hub. Prefer `{language}:{version}-alpine` images. If no alpine variant exists, use the smallest official image (slim, bookworm-slim, etc.). Record the exact image tag including patch version and alpine/distro version (e.g. `rust:1.80.1-alpine3.20`).

2. **HTTP framework**: Identify the most popular, lightweight HTTP server library for the language that can:
   - Listen on a configurable port
   - Handle all HTTP methods (GET, POST, PUT, DELETE, PATCH, OPTIONS, HEAD)
   - Read request headers, body (as bytes/text/JSON), method, URL path, query string
   - Set response status code, headers, and body (text and binary)
   - Run with good performance in a container

3. **Compiled vs interpreted**: Determine whether the runtime needs:
   - **Interpreted**: Build step installs dependencies, start step runs server directly
   - **Compiled**: Build step compiles to binary, start step runs the binary

4. **Dependency management**: Identify the standard package manager and dependency file format (e.g. Cargo.toml, mix.exs, Package.swift, cabal.project).

5. **Code formatter**: Find the standard code formatter for the language and how to install/run it.

6. **Test HTTP client library**: Find a popular HTTP client library that can be added as a dependency in the test function (used to call `https://dummyjson.com/todos/{id}`).

### Phase 2: Generate Files

Create ALL of the following files. Every file is required for the runtime to work.

#### 2.1 Runtime Dockerfile: `runtimes/{language}/{language}.dockerfile`

This is the language-specific Dockerfile fragment that gets INCLUDEd between `base-before` and `base-after`. Pattern:

```dockerfile
RUN apk add bash
# Add any language-specific system dependencies here

ENV OPEN_RUNTIMES_ENTRYPOINT={default_entrypoint}
# For interpreted: main source file (e.g. index.js, main.py)
# For compiled: main source file before compilation (e.g. main.go, main.rs)
```

For compiled languages, also copy helpers and any build tool configs. For interpreted languages, copy dependency files, install runtime dependencies, then copy the rest.

Study these existing examples to match the exact pattern:
- **Interpreted (simple)**: `runtimes/python/python.dockerfile`, `runtimes/ruby/ruby.dockerfile`
- **Compiled (simple)**: `runtimes/go/go.dockerfile`
- **Compiled (Gradle-based)**: `runtimes/kotlin/kotlin.dockerfile`

#### 2.2 Version Dockerfile: `runtimes/{language}/versions/{version}/Dockerfile`

```dockerfile
# syntax = devthefuture/dockerfile-x:1.4.2
FROM {exact_docker_image_tag}

INCLUDE ./base-before
INCLUDE ./{language}
INCLUDE ./base-after
```

#### 2.3 Server Implementation: `runtimes/{language}/versions/latest/src/`

The server **must** implement this exact protocol. Study multiple existing servers (Go's `main.go`, Python's `server.py`, Ruby's `server.rb`, Kotlin's `Server.kt`) to understand the pattern precisely.

**Startup:**
- Print `HTTP server successfully started!` to stdout when ready (this string is detected by `helpers/start.sh` for telemetry)
- Listen on `0.0.0.0:3000`
- Handle ALL HTTP methods on ALL paths

**Built-in endpoints (handle before user function):**
- `GET /__opr/health` -> respond `200 OK` with body `OK`
- `GET /__opr/timings` -> read `/mnt/telemetry/timings.txt` and respond with its content as `text/plain; charset=utf-8`

**Request processing:**
1. Read `x-open-runtimes-timeout` header. If present, parse as integer. If invalid or negative, respond 500 with `Header "x-open-runtimes-timeout" must be an integer greater than 0.`
2. Read `x-open-runtimes-secret` header and compare with `OPEN_RUNTIMES_SECRET` env var. If env var is non-empty and doesn't match, respond 500 with `Unauthorized. Provide correct "x-open-runtimes-secret" header.`
3. Read request body as bytes
4. Collect headers into a map, filtering out any starting with `x-open-runtimes-` (lowercase comparison)
5. Parse `OPEN_RUNTIMES_HEADERS` env var as JSON object and merge into headers (lowercase keys)
6. Parse host, port, scheme, path, query string, full URL from request + `x-forwarded-proto` header
7. Create context object with `req` (request), `res` (response builder), and logger
8. Override native stdout/stderr to capture into logger
9. Dynamically load and call user function from `OPEN_RUNTIMES_ENTRYPOINT` env var
10. If timeout is set, enforce it. On timeout: log `Execution timed out.` as error, respond empty body with 500
11. If function doesn't return a response: log `Return statement missing. return context.res.empty() if no response is expected.` as error, respond empty body with 500
12. Revert native stdout/stderr
13. Process response: lowercase header keys, add `content-type: text/plain` if missing, append `; charset=utf-8` if no charset specified (skip for multipart), filter out `x-open-runtimes-` response headers, add `x-open-runtimes-log-id` header
14. On any exception: write stack trace to error log, respond empty body with 500

**Request object properties:**
- `method` (string) - HTTP method
- `scheme` (string) - from `x-forwarded-proto` header, default `http`
- `host` (string) - from `Host` header, without port
- `port` (int) - from `Host` header, default 80/443 based on scheme
- `path` (string) - URL path
- `query` (map string->string) - parsed query parameters
- `queryString` (string) - raw query string
- `headers` (map string->string) - lowercase keys
- `bodyBinary` (bytes) - raw body bytes
- `bodyText` (string) - body as text
- `bodyJson` (map) - body parsed as JSON
- `bodyRaw` (string) - alias for bodyText
- `body` (auto) - bodyJson if content-type is application/json, otherwise bodyText
- `url` (string) - full reconstructed URL

**Response builder methods:**
- `binary(bytes, statusCode=200, headers={})` -> RuntimeOutput
- `text(string, statusCode=200, headers={})` -> RuntimeOutput (calls binary with encoded string)
- `send(string, statusCode=200, headers={})` -> RuntimeOutput (alias for text)
- `json(map, statusCode=200, headers={})` -> RuntimeOutput (serializes to JSON, sets content-type header)
- `empty()` -> RuntimeOutput (empty body, 204)
- `redirect(url, statusCode=301, headers={})` -> RuntimeOutput (sets location header, empty body)

**Logger implementation:**
- Takes `status` (from `x-open-runtimes-logging` header) and `id` (from `x-open-runtimes-log-id` header)
- If status is `enabled` or empty string, logging is enabled
- If id is empty and enabled: generate random 20-char hex ID (8 hex chars from unix timestamp seconds + 5 hex chars from microseconds + 7 random hex chars)
- If id is empty and env `OPEN_RUNTIMES_ENV=development`, use `dev` as id
- Write logs to `/mnt/logs/{id}_logs.log` and errors to `/mnt/logs/{id}_errors.log`
- Truncate messages at 8000 characters with suffix `... Log truncated due to size limit (8000 characters)`
- `write(messages[], type, native)` - type is `log` or `error`
- On first native log detection, prepend: `Native logs detected. Use context.log() or context.error() for better experience.`
- Support serializing maps, lists, sets as JSON; other types as strings
- Multiple arguments separated by space
- Each context.log()/context.error() call appends a newline after the message

**Context object:**
- `req` - RuntimeRequest
- `res` - RuntimeResponse
- `log(vararg messages)` - calls logger.write(messages, "log") then logger.write(["\n"], "log")
- `error(vararg messages)` - calls logger.write(messages, "error") then logger.write(["\n"], "error")

#### 2.4 Helper Scripts: `runtimes/{language}/versions/latest/helpers/`

**Required: `server.sh`** - How to start the server

For interpreted:
```bash
#!/bin/bash
set -e
shopt -s dotglob
{command_to_run_server}
# e.g.: python src/server.py
# e.g.: ruby src/server.rb
# e.g.: node src/server.js
```

For compiled:
```bash
#!/bin/bash
set -e
shopt -s dotglob
src/function/server
# or: java -jar src/function/{jarname}
```

**Optional helpers** (only create if needed, otherwise the global empty stubs are used):

- `prepare-build.sh` - Runtime-specific setup before user's install command runs. Used for things like:
  - Copying user code into the server's source tree (compiled languages)
  - Setting up virtual environments (Python)
  - Merging user dependency files with server dependency files

- `prepare-compile.sh` - Compile step for compiled languages:
  - Run the compiler/build tool
  - Move binary output to `/usr/local/build/`

- `prepare-packing.sh` - Adjust build output before tarball creation:
  - Move compiled artifacts to clean locations
  - Set `OPEN_RUNTIMES_CLEANUP` if needed

- `prepare-start.sh` - Pre-start preparation:
  - Copy server dependencies to runtime location
  - Activate virtual environments

#### 2.5 Dependency File

Create the appropriate dependency file in `runtimes/{language}/versions/latest/`:
- `package.json` (Node/Bun), `requirements.txt` (Python), `Gemfile` (Ruby), `go.mod` (Go), `build.gradle` (Kotlin/Java), `Cargo.toml` (Rust), `mix.exs` (Elixir), etc.

Include only the HTTP framework dependency and any essential libraries (JSON parsing, async runtime if needed).

#### 2.6 Dotfiles: `runtimes/{language}/versions/latest/`

**`.dockerignore`:**
```
example/
```

**`.gitignore`:**
```
# Open-runtime related
example/code.tar.gz

# Language-specific ignores (build artifacts, caches, etc.)
{language_specific_patterns}

# OS
## Mac
.DS_Store
```

#### 2.7 README: `runtimes/{language}/README.md`

Follow the pattern from existing READMEs (e.g. `runtimes/go/README.md`, `runtimes/python/README.md`). Include:
- Runtime title and badges
- Quick example of a user function
- Build and run Docker commands
- Request/response format
- Notes about the runtime

#### 2.8 Test Function: `tests/resources/functions/{language}/latest/`

Create the test entrypoint file (e.g. `tests.rs`, `tests.ex`, `tests.lua`). This MUST implement ALL of the following actions, switched on the `x-action` request header. Study `tests/resources/functions/go/latest/tests.go` and `tests/resources/functions/python/latest/tests.py` for the exact pattern.

**Required actions:**

```
plaintextResponse     -> return context.res.text("Hello World \u{1F44B}")
jsonResponse          -> return context.res.json({"json": true, "message": "Developers are awesome."})
customCharsetResponse -> return context.res.text("\u{00C5}\u{00C6}", 200, {"content-type": "text/plain; charset=iso-8859-1"})
uppercaseCharsetResponse -> return context.res.text("\u{00C5}\u{00C6}", 200, {"content-type": "TEXT/PLAIN"})
multipartResponse     -> return multipart body with boundary=12345 and content-type multipart/form-data
redirectResponse      -> return context.res.redirect("https://github.com/")
emptyResponse         -> return context.res.empty()
noResponse            -> call context.res.text() but DON'T return it. Then: context.error("Return statement missing...") and return context.res.text("", 500)
doubleResponse        -> call context.res.text("ignored") then RETURN context.res.text("This should be returned.")
enforcedHeaders       -> return JSON with x-custom, x-custom-uppercase, x-open-runtimes-custom from req.headers
headersResponse       -> return text "OK" with headers: first-header, second-header (from x-open-runtimes-custom-in-header), cookie, x-open-runtimes-custom-out-header
statusResponse        -> return context.res.text("FAIL", 404)
requestMethod         -> return context.res.text(context.req.method)
requestUrl            -> return JSON: {url, port, path, query, queryString, scheme, host}
requestHeaders        -> return context.res.json(context.req.headers)
requestBodyText       -> return context.res.text(context.req.bodyText)
requestBodyJson       -> return context.res.json(context.req.bodyJson)
requestBodyBinary     -> return context.res.binary(context.req.bodyBinary)
requestBodyTextAuto   -> return context.res.text(context.req.body) (when body is text)
requestBodyJsonAuto   -> return context.res.json(context.req.body) (when body is JSON)
binaryResponse1       -> return context.res.binary(bytes [0, 10, 255])
binaryResponse2       -> return context.res.binary(bytes [0, 20, 255])
binaryResponse3       -> return context.res.binary(bytes [0, 30, 255])
binaryResponse4       -> return context.res.binary(bytes [0, 40, 255])
binaryResponse5       -> return context.res.binary(bytes [0, 50, 255])
binaryResponseLarge   -> compute MD5 of bodyBinary, return lowercase hex digest with x-method header
envVars               -> return JSON: {var: env("CUSTOM_ENV_VAR"), emptyVar: env("NOT_DEFINED_VAR")}
logs                  -> println("Native log"), context.log("Debug log"), context.error("Error log"),
                         context.log("Log+With+Plus+Symbol"),
                         context.log(42), context.log(4.2), context.log(true),
                         context.log(["arrayValue"]),
                         context.log({"objectKey": "objectValue"}),
                         context.log("A" * 9000), context.error("B" * 9000),
                         return context.res.text("")
library               -> fetch https://dummyjson.com/todos/{bodyRaw} using HTTP client library,
                         return context.res.json({"todo": parsed_response})
timeout               -> context.log("Timeout start."), sleep 3 seconds, context.log("Timeout end."),
                         return context.res.text("Successful response.")
deprecatedMethods     -> return context.res.send(context.req.bodyRaw)
deprecatedMethodsUntypedBody -> return context.res.send("50")
spreadOperatorLogs    -> context.log("engine:", "open-runtimes"), context.error("engine:", "open-runtimes"),
                         return context.res.text("OK")
errorTest             -> context.log("Before error..."), throw/raise/panic "Error!"
default/unknown       -> throw/raise/panic "Unknown action"
```

Also create:
- The test dependency file (e.g. `go.mod`, `requirements.txt`, `Cargo.toml`) including the HTTP client library
- A `no-export` test file (e.g. `no-export.{ext}`) that exists but does NOT export/define a `main` function with the correct signature. This tests error handling for missing function exports.

#### 2.9 PHP Test Class: `tests/Serverless/{Language}.php`

```php
<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class {Language} extends Serverless
{
    public function testSetCookie(): void
    {
        self::assertTrue(true); // Disable test till implemented
    }

    public function testDeprecatedMethodsBytesBody(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'deprecatedMethodsBytesBody']);
        self::assertEquals(500, $response['code']);
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }
}
```

Note: `testDeprecatedMethodsBytesBody` should return 500/"Unknown action" for compiled/typed languages where `send()` only accepts String (the test sends bytes via body but the action tries to return bodyRaw as string - if the language auto-converts, remove this override). For interpreted/dynamically-typed languages that handle this naturally, you may not need this override.

#### 2.10 CI Configuration: `ci/runtimes.toml`

Add a new entry in alphabetical order among the runtime entries (before the framework entries like `[astro]`, `[sveltekit]`, etc.):

```toml
[{language}]
entry = "{test_entrypoint}"
entry_no_export = "{no_export_file}"
versions = ["{version}"]
commands = { install = "{install_command}", start = "bash helpers/server.sh" }
formatter = { prepare = "{formatter_install}", check = "{formatter_check}", write = "{formatter_write}" }
tools = "{tools_check_command}"
test = "Serverless/{Language}.php"
```

### Phase 3: Verify

After generating all files, perform these verification steps:

1. **File inventory check** - Confirm every required file exists:
   - `runtimes/{language}/{language}.dockerfile`
   - `runtimes/{language}/README.md`
   - `runtimes/{language}/versions/latest/src/` (server + logger + types)
   - `runtimes/{language}/versions/latest/helpers/server.sh`
   - `runtimes/{language}/versions/latest/{dependency_file}`
   - `runtimes/{language}/versions/latest/.dockerignore`
   - `runtimes/{language}/versions/latest/.gitignore`
   - `runtimes/{language}/versions/{version}/Dockerfile`
   - `tests/resources/functions/{language}/latest/{test_file}`
   - `tests/resources/functions/{language}/latest/{no_export_file}`
   - `tests/resources/functions/{language}/latest/{test_dependency_file}`
   - `tests/Serverless/{Language}.php`
   - Entry in `ci/runtimes.toml`

2. **Protocol compliance** - Review the server implementation against the protocol spec above. Check every endpoint, every header, every edge case.

3. **Test completeness** - Verify every action from the test action list is implemented in the test function.

4. **Build test** - Run `bash tests.sh {language}-{version}` to build and test the runtime. Fix any failures.

## Critical Rules

- The server MUST print `HTTP server successfully started!` to stdout when ready
- The server MUST listen on port 3000
- ALL response header keys MUST be lowercased
- `x-open-runtimes-*` headers MUST be filtered from both request headers passed to user and response headers returned
- The `x-open-runtimes-log-id` header MUST always be set on the response
- Log IDs MUST be exactly 20 hex characters (unless in dev mode where it's `dev`)
- Logs MUST be truncated at 8000 characters
- JSON responses MUST serialize nulls
- Content-type MUST default to `text/plain; charset=utf-8` when not set
- Multipart content-types MUST NOT have charset appended
- Content-type values MUST be lowercased (except multipart)
- The `empty()` response MUST return status 204
- Error responses from the runtime itself (not user code) MUST return status 500 with empty body
- Module-not-found errors MUST return status 503
- The Dockerfile MUST use `# syntax = devthefuture/dockerfile-x:1.4.2` as the first line
- The Dockerfile MUST use the three-INCLUDE pattern: `base-before`, `{language}`, `base-after`
