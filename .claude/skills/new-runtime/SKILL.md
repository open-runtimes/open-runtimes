---
name: new-runtime
description: Add an entirely new function runtime to open-runtimes. Generates all required files (server, Dockerfile, helpers, tests, CI config) so the runtime passes the test suite in one shot.
disable-model-invocation: true
argument-hint: "{language} {version}"
allowed-tools: Read Write Edit Glob Grep Bash Agent WebSearch WebFetch
---

# New Runtime Generator

Generate a complete, test-passing runtime for the open-runtimes project.

The canonical contributor guides are `docs/add-runtime.md` (layout, Dockerfile,
lifecycle hooks, registration) and `docs/testing.md` (local harness). Read both
before generating anything; when this skill and those docs disagree, the docs
win — except where the generated runtime would then fail `make test`, in which
case the suite in `tests/Serverless.php` and `tests/Base.php` is the real
contract and the doc is a bug worth fixing in the same change.


## Arguments

The user provides: `{language} {version}`

- `language` - The programming language name (e.g. `rust`, `zig`, `elixir`, `lua`, `perl`, `r`, `haskell`, `scala`, `ocaml`)
- `version` - The initial version to support (e.g. `1.80`, `3.12`, `0.13`)

## Process

### Phase 1: Research

Before writing any code, gather the information needed to make correct decisions. Run these research steps **in parallel** using subagents:

1. **Docker image**: Find the official alpine-based Docker image for the language + version on Docker Hub. Prefer `{language}:{version}-alpine` images. If no alpine variant exists, use the smallest official image (slim, bookworm-slim, etc.). Record the exact image tag including patch version and alpine/distro version (e.g. `rust:1.80.1-alpine3.20`), and record whether the image is Alpine or Debian based — the Dockerfile package command depends on it (see 2.1).

   Check the image's manifest for **every platform the bake file will publish**. `ci/bake.ts` defaults to `linux/amd64` and `linux/arm64`, while `make test` builds only `linux/x86_64` unless `TEST_PLATFORM` says otherwise, so an image with no arm64 manifest passes the whole local workflow and still fails to publish. Either pick a base that covers both, or set an intentional `platforms` override on the version in `ci/runtimes.toml` (as the deno versions do) and say why.

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

#### 2.1 Runtime Dockerfile: `runtimes/{language}/Dockerfile`

One family `Dockerfile` serves every version. It starts from the `BASE_IMAGE`
build arg (supplied per version from `ci/runtimes.toml`), `INCLUDE`s the shared
base fragments from `docker/`, installs the language's system packages, and
declares the command that launches the server:

```dockerfile
# syntax=docker/dockerfile:1
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

RUN apk add --no-cache bash
# Alpine bases only. On a slim/bookworm base use
# `RUN apt-get update && apt-get install -y --no-install-recommends bash && rm -rf /var/lib/apt/lists/*`
# instead, or branch on `/etc/alpine-release` the way `docker/base-before.dockerfile` does.
# Add any language-specific system dependencies here

ENV OPEN_RUNTIMES_ENTRYPOINT={default_entrypoint}
# For interpreted: main source file (e.g. index.js, main.py)
# For compiled: main source file before compilation (e.g. main.go, main.rs)

ENV OPEN_RUNTIMES_SERVER_COMMAND="{command that starts the server}"
# For interpreted: the interpreter on the server source (e.g. python3 src/server.py)
# For compiled: the built binary (e.g. src/function/server)

INCLUDE ./docker/base-after
```

`bun ci/bake.ts` inlines `docker/base-before.dockerfile` and
`docker/base-after.dockerfile`, which create `/mnt/code`, `/mnt/logs`,
`/mnt/telemetry`, `/usr/local/build`, assemble `/usr/local/server` from the
`helpers`, `shared`, `latest` and `version` build contexts, set the default
env, and `EXPOSE 3000`. Always keep both INCLUDEs.

Study these existing examples to match the exact pattern:
- **Interpreted (simple)**: `runtimes/python/Dockerfile`, `runtimes/ruby/Dockerfile`
- **Compiled (simple)**: `runtimes/go/Dockerfile`, `runtimes/rust/Dockerfile`
- **Compiled (Gradle-based)**: `runtimes/kotlin/Dockerfile`

#### 2.2 Per-version overlay (optional): `runtimes/{language}/versions/{version}/`

There is no per-version Dockerfile. The base image for each version comes from
the build table in `ci/runtimes.toml` (see 2.10). Only create
`runtimes/{language}/versions/{version}/` when a version needs files that differ
from `versions/latest/` (a custom `build.gradle`, different sources); anything
placed there overlays `versions/latest/` at image-build time.

#### 2.3 Server Implementation: `runtimes/{language}/versions/latest/src/`

The server **must** implement this exact protocol. Study multiple existing servers (Go's `main.go`, Python's `server.py`, Ruby's `server.rb`, Kotlin's `Server.kt`) to understand the pattern precisely.

**Startup:**
- Print `HTTP server successfully started!` to stdout when ready (this string is detected by `helpers/start.sh` for telemetry)
- Listen on `0.0.0.0:3000`
- Handle ALL HTTP methods on ALL paths

**Shutdown:**
- On SIGTERM, use the HTTP framework's graceful shutdown: stop accepting new
  connections and let in-flight requests finish before exiting. Tini and
  `helpers/start.sh` forward the signal and reap children, but nothing in the
  shared helpers drains requests for you — a server that exits immediately
  passes every protocol rule above and still fails verification, because
  `bun ci/test.ts` sends SIGTERM during the three-second `timeout` fixture and
  requires that request to complete and the container to exit 0 or 143.
  Read `docs/shutdown.md` before implementing this.

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

#### 2.4 Lifecycle Hooks: `runtimes/{language}/versions/latest/hooks/`

The global lifecycle runner (`helpers/lifecycle/build.sh` and
`helpers/lifecycle/start.sh`) is shared by every runtime. A runtime customizes
it by dropping scripts into `versions/latest/hooks/`. A missing hook is a no-op.
Hooks are **sourced, not executed**, so they can export environment (activate a
virtualenv, set `OPEN_RUNTIMES_CLEANUP`) for later phases.

Every hook starts with:
```bash
#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob
```

**Build** (`helpers/build.sh "<install command>"`):

- `build-prepare.sh` - after the build cache is restored and `/mnt/code` is copied to `/usr/local/build`, before the install command runs. Used for creating virtual environments (Python) or merging user dependency files with server dependency files.
- `compile.sh` - compiled languages only: copy `/usr/local/build` into the server's source tree, run the compiler, move the binary back to `/usr/local/build/` (see `runtimes/rust/versions/latest/hooks/compile.sh`, `runtimes/go/versions/latest/hooks/compile.sh`).
- `pack.sh` - prune the build output before it is archived; set `OPEN_RUNTIMES_CLEANUP` if needed.

**Start** (`helpers/start.sh "<start command>"`):

- `start-prepare.sh` - after the archive is extracted, before the server starts: move dependencies or binaries into place, activate environments (see `runtimes/python/versions/latest/hooks/start-prepare.sh`).
- `server.sh` - only when starting the server takes more than the single `OPEN_RUNTIMES_SERVER_COMMAND` from the Dockerfile (worker counts, `exec` with computed flags). When present it takes precedence over `OPEN_RUNTIMES_SERVER_COMMAND` in `helpers/server.sh`. It must print `HTTP server successfully started!` before `exec`ing the server if the server itself does not (see `runtimes/python/versions/latest/hooks/server.sh`).

Most interpreted runtimes need no hooks at all; a compiled runtime typically needs only `compile.sh`.

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
noResponse            -> call context.res.text("This should be ignored, as it is not returned.") and
                         DON'T return it, so the runtime's own missing-return detection produces the
                         error log and the empty 500 (see python's tests.py). Only when the language
                         cannot express a missing return (Go, Rust, Java) may the fixture simulate it
                         by logging "Return statement missing. return context.res.empty() if no
                         response is expected." and returning context.res.text("", 500)
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
deprecatedMethodsBytesBody -> return context.res.send(<the fixed PNG bytes>, 200, {"content-type": "image/png"}).
                         Copy the base64 blob from tests/resources/functions/python/latest/tests.py;
                         testDeprecatedMethodsBytesBody asserts 200, a content-type starting with
                         image/png, and md5(body) == 2a8fdeea08e939e9a7c05653544a1374.
                         Omit this action only if send() in this language cannot carry bytes at all
                         (see 2.9)
spreadOperatorLogs    -> context.log("engine:", "open-runtimes"), context.error("engine:", "open-runtimes"),
                         return context.res.text("OK")
errorTest             -> context.log("Before error..."), throw/raise/panic "Error!"
default/unknown       -> throw/raise/panic "Unknown action"
```

Also create:
- The test dependency file (e.g. `go.mod`, `requirements.txt`, `Cargo.toml`) including the HTTP client library
- A `no-export` test file (e.g. `no-export.{ext}`) that exists but does NOT export/define a `main` function with the correct signature. This tests error handling for missing function exports. Create it **only** if the runtime loads the entrypoint dynamically and can therefore start and answer 503; skip it, and leave `entry_no_export` out of `ci/runtimes.toml`, for statically linked handlers (see 2.10).

#### 2.9 PHP Test Class: `tests/Serverless/{Language}.php`

Start from an empty subclass and add an override only for a capability the
runtime genuinely cannot provide:

```php
<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class {Language} extends Serverless
{
}
```

The inherited suite in `tests/Serverless.php` is the contract. **Do not neuter
an inherited test with `self::assertTrue(true)` to get green** — implement the
fixture action instead. Two overrides exist today and both are debts, not
patterns to copy:

- `testSetCookie` is stubbed out in every runtime because no fixture implements
  the `setCookie` / `setCookie2` actions yet. If your runtime's response builder
  can emit repeated `set-cookie` headers, implement the actions and inherit the
  real test rather than adding a fourteenth stub.
- `testDeprecatedMethodsBytesBody` is overridden to expect `500` / `Unknown
  action` only in runtimes whose `send()` cannot carry bytes (Go), where the
  fixture deliberately omits the action. If `send()` in your language accepts
  bytes, ship the `deprecatedMethodsBytesBody` action from 2.8 and inherit the
  real assertions (200, `image/png`, md5 `2a8fdeea08e939e9a7c05653544a1374`).

An override that asserts a *different* observable outcome for a genuinely
unsupported capability is fine; an override that asserts nothing is not.

#### 2.10 CI Configuration: `ci/runtimes.toml`

Add a runtime section in alphabetical order among the runtime entries (before the framework entries like `[astro]`, `[sveltekit]`, etc.), then a build table:

```toml
[{language}]
entry = "{test_entrypoint}"
entry_no_export = "{no_export_file}"   # omit when the runtime cannot express a missing export
versions = ["{version}"]
commands = { install = "{install_command}", start = "bash helpers/server.sh" }
formatter = { prepare = "{formatter_install}", check = "{formatter_check}", write = "{formatter_write}" }
tools = "{tools_check_command}"
test = "Serverless/{Language}.php"

[{language}.build.versions]
"{version}" = { base = "{exact_docker_image_tag}" }
```

`entry_no_export` is **optional and must be omitted for runtimes that bind the
handler at compile time**. When it is present the harness builds that variant
before running PHPUnit and expects a server that answers 503; a statically
linked handler (Rust, Go, .NET, Swift, C++, Dart) simply fails to compile
instead, so those runtimes deliberately have no `entry_no_export` — follow
them rather than the template comment. Only declare it, and ship the matching
`no-export` fixture from 2.8, when the entrypoint is loaded dynamically.

Always pin the most specific base image (patch version + distro). Optional
per-version keys: `args` (Dockerfile build args), `platforms`, `version_dir`.
Each `[{language}.build.versions]` key publishes `openruntimes/{language}:v5-{version}`.

Then regenerate the bake file:

```bash
bun ci/bake.ts
```

CI rejects the change if `docker-bake.json` is stale or if the `versions` list and the build version list drift apart.

#### 2.11 README images table

Add the new image to the Images table in the root `README.md`, sorted alphabetically.

### Phase 3: Verify

After generating all files, perform these verification steps:

1. **File inventory check** - Confirm every required file exists:
   - `runtimes/{language}/Dockerfile`
   - `runtimes/{language}/README.md`
   - `runtimes/{language}/versions/latest/src/` (server + logger + types)
   - `runtimes/{language}/versions/latest/hooks/` (only the hooks the runtime needs; `compile.sh` for compiled languages)
   - `runtimes/{language}/versions/latest/{dependency_file}`
   - `runtimes/{language}/versions/latest/.dockerignore`
   - `runtimes/{language}/versions/latest/.gitignore`
   - `tests/resources/functions/{language}/latest/{test_file}`
   - `tests/resources/functions/{language}/latest/{no_export_file}`
   - `tests/resources/functions/{language}/latest/{test_dependency_file}`
   - `tests/Serverless/{Language}.php`
   - Runtime section and build table in `ci/runtimes.toml`
   - Regenerated `docker-bake.json`
   - Row in the root `README.md` Images table

2. **Protocol compliance** - Review the server implementation against the protocol spec above. Check every endpoint, every header, every edge case.

3. **Test completeness** - Verify every action from the test action list is implemented in the test function.

4. **Build test** - Run `make test ID={language}-{version}` (which is `bun ci/test.ts {language}-{version}`) to build the image, run the formatter check, stage fixtures and run the PHPUnit suite. Use `--skip-image` to iterate without rebuilding. Fix any failures. See `docs/testing.md`.

## Critical Rules

- The server MUST print `HTTP server successfully started!` to stdout when ready
- The server MUST listen on port 3000
- ALL response header keys MUST be lowercased
- `x-open-runtimes-*` headers MUST be filtered from both request headers passed to user and response headers returned
- The `x-open-runtimes-log-id` header MUST always be set on the response
- A **generated** log ID MUST be exactly 20 hex characters; in development
  (`OPEN_RUNTIMES_ENV=development`) it is `dev`. A caller-supplied
  `x-open-runtimes-log-id` MUST be echoed back unchanged, whatever its shape
  (`customLogs`, `myLog`), and when logging is disabled the response header MUST
  be empty — `testLogs` asserts all three
- Logs MUST be truncated at 8000 characters
- JSON responses MUST serialize nulls
- Content-type MUST default to `text/plain; charset=utf-8` when not set
- Multipart content-types MUST NOT have charset appended
- Content-type values MUST be lowercased (except multipart)
- The `empty()` response MUST return status 204
- Errors raised while loading or executing user code MUST return status 500 with
  an empty body (the detail goes to the error log). The runtime's own **request
  validation** responses are the exception and MUST carry their message in the
  body: `Header "x-open-runtimes-timeout" must be an integer greater than 0.` and
  `Unauthorized. Provide correct "x-open-runtimes-secret" header.` — `testTimeout`,
  `testWrongSecret` and `testEmptySecret` assert those exact strings
- Module-not-found errors MUST return status 503
- The Dockerfile MUST start with `# syntax=docker/dockerfile:1`, take `ARG BASE_IMAGE`, and keep both `INCLUDE ./docker/base-before` and `INCLUDE ./docker/base-after`
- The Dockerfile MUST set `OPEN_RUNTIMES_SERVER_COMMAND` (or the runtime MUST ship `hooks/server.sh`)
- `docker-bake.json` MUST be regenerated with `bun ci/bake.ts` after editing `ci/runtimes.toml`
- The server MUST drain in-flight requests on SIGTERM and then exit cleanly (see `docs/shutdown.md`)
- Every platform in the version's bake target MUST have a manifest on the chosen base image
