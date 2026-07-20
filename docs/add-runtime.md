# Creating a new functions runtime 🏃

This document is part of the Open Runtimes contributors' guide. Before you continue reading this document make sure you have read the [Code of Conduct](https://github.com/open-runtimes/open-runtimes/blob/main/CODE_OF_CONDUCT.md) and the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md).

> Adding a **new version** to an existing runtime is a much smaller, config-only
> change. See [add-version.md](add-version.md). This guide is for adding a
> brand-new language.

## 1. Prerequisites

For a runtime to work, two prerequisites **must** be met due to the way Open Runtimes's Runtime Execution Model works:

 - [ ] The language must be able to run a web server that serves JSON and text on port `3000`.
 - [ ] The runtime must be packageable into a Docker container.

Both compiled and interpreted languages work with Open Runtimes; they only differ in which build hooks they implement (see [§3](#3-the-build-and-start-lifecycle)).

### 1.1 Fork the Open Runtimes repository

Fork [the repository](https://github.com/open-runtimes/open-runtimes), clone your fork, and create a `feat-XXX-YYY-runtime` branch from `main` (`XXX` = issue ID, `YYY` = runtime name).

```bash
git clone COPIED_URL
git checkout -b feat-XXX-YYY-runtime
```

## 2. Runtime layout

Each runtime lives in `runtimes/<runtime>/`. There is **one family `Dockerfile`**
that serves every version; versioned source lives under `versions/`. Using Rust
as a compiled example and Node as an interpreted one, a runtime looks like:

```
runtimes/<runtime>/
├── Dockerfile               # one Dockerfile for all versions (see §2.1)
├── README.md                # short description + notes for this runtime
├── versions/
│   └── latest/              # source shared by every version
│       ├── src/             # the runtime HTTP server (server.js, main.rs, …)
│       └── hooks/           # optional build/start lifecycle hooks (see §3)
│   └── <version>/           # OPTIONAL: files that overlay latest/ for one version
└── versions/latest/...
```

At image-build time the container's `/usr/local/server` directory is assembled
by overlaying four [named build contexts](https://docs.docker.com/build/building/context/), later layers winning:

1. `helpers` → the repo's `helpers/` (the global lifecycle runner — you don't touch this)
2. `shared` → `runtimes/<shared family>/` (e.g. `javascript` for node/bun/deno), or empty
3. `latest` → `runtimes/<runtime>/versions/latest/`
4. `version` → `runtimes/<runtime>/versions/<version>/` extras, or empty

User code is mounted at `/mnt/code`; the build directory is `/usr/local/build`.

### 2.1 Writing the Dockerfile

The Dockerfile is intentionally thin. It starts from a `BASE_IMAGE` build arg
(the actual version is supplied from `ci/runtimes.toml`, see [§4](#4-register-the-runtime)),
`INCLUDE`s the shared base fragments, installs any system packages the language
needs, and declares the command that starts the server. Real example (`runtimes/rust/Dockerfile`):

```dockerfile
# syntax=docker/dockerfile:1
ARG BASE_IMAGE
FROM ${BASE_IMAGE}

INCLUDE ./docker/base-before

ENV OPEN_RUNTIMES_ENTRYPOINT=main.rs

RUN apk update && apk add --no-cache bash musl-dev openssl-dev pkgconfig curl

ENV OPEN_RUNTIMES_SERVER_COMMAND="src/function/rust_runtime"

INCLUDE ./docker/base-after
```

Key points:

- **`INCLUDE ./docker/base-before` / `./docker/base-after`** are resolved by
  `bun ci/bake.ts`, which inlines `docker/base-before.dockerfile` and
  `docker/base-after.dockerfile`. These create `/mnt/code`, `/mnt/logs`,
  `/mnt/telemetry`, `/usr/local/build`, assemble `/usr/local/server` from the
  build contexts, set default env (`OPEN_RUNTIMES_SECRET`, `OPEN_RUNTIMES_ENV`,
  `OPEN_RUNTIMES_HEADERS`), and `EXPOSE 3000`. Always keep both INCLUDEs.
- **`OPEN_RUNTIMES_SERVER_COMMAND`** is the command that launches your HTTP
  server. For interpreted runtimes it invokes the interpreter on your server
  source (e.g. node sets `node ... src/server.js`); for compiled runtimes it
  points at the built binary.
- **`OPEN_RUNTIMES_ENTRYPOINT`** is the default user entrypoint file name. It
  can be overridden per deployment.
- Prefer small `alpine` base images where the language allows it.

## 3. The build and start lifecycle

The global lifecycle runner lives in `helpers/lifecycle/` and is the same for
every runtime. Your runtime customizes it by dropping scripts into
`versions/latest/hooks/`. A missing hook means that phase is a no-op — most
interpreted runtimes need **no hooks at all** (the shared `javascript` family
provides the JS ones).

Hooks are **sourced, not executed**, so a hook can export environment that later
phases see (e.g. activating a virtualenv, or setting `OPEN_RUNTIMES_CLEANUP`).

**Build** (`helpers/lifecycle/build.sh`, invoked as `helpers/build.sh "<install command>"`):

| Phase | What happens | Hook |
|---|---|---|
| Stage | restore build cache, copy `/mnt/code` → `/usr/local/build` | `build-prepare` |
| Install | run the user/install command in `/usr/local/build` | — |
| Compile | (compiled langs) build the binary | `compile` |
| Pack | prune/clean build output | `pack` |
| Archive | package `/usr/local/build` → `/mnt/code/code.tar.gz` by default, or `/mnt/code/code.sqfs` when `OPEN_RUNTIMES_BUILD_COMPRESSION=squashfs`; write `.open-runtimes` metadata, save build cache | — |

**Start** (`helpers/lifecycle/start.sh`, invoked as `helpers/start.sh "<start command>"`):

| Phase | What happens | Hook |
|---|---|---|
| Extract | unpack `code.sqfs` when present, otherwise unpack legacy `code.tar.gz`, `code.tar`, or `code.gz` | — |
| Prepare | move deps/binaries into place, activate envs | `start-prepare` |
| Serve | run the start command and watch for the server-ready line | — |

A **compiled** runtime typically only needs a `compile` hook (and sometimes a
`pack` hook to keep only the binary). For example, `runtimes/rust/versions/latest/hooks/compile.sh`
copies user code into `src/function/`, runs `cargo build --release`, and moves
the binary into `/usr/local/build`. An **interpreted** runtime usually needs no
hooks beyond what its shared family already provides.

> Useful paths: `/mnt/code` (mounted user code + the output `code.tar.gz` or `code.sqfs`),
> `/usr/local/build` (build working dir), `/usr/local/server` (the runtime
> itself), `/mnt/telemetry` (timing files), `/mnt/logs`.

Build output compression is controlled by `OPEN_RUNTIMES_BUILD_COMPRESSION`.
The default is `gzip` for downloadable artifact compatibility. `auto` picks by
output size — skipping compression under 5MB and using `gzip` at or above it —
and explicit values `squashfs`, `gzip`, `zstd`, `none`, and `skip` are supported.
`none` writes an uncompressed `code.tar.gz`, whereas `skip` writes no archive at
all — the raw build output is left in the output dir with a `.extracted` marker so
start-up symlinks it directly. SquashFS packages with LZ4 for faster packaging and
extraction; downloaded SquashFS outputs can be extracted with `unsquashfs -d output code.sqfs`.

## 4. Writing the runtime server

Your server in `versions/latest/src/` must run an HTTP server on `0.0.0.0:3000`.
The contract (see `runtimes/node/versions/latest/src/server.js` for the
reference implementation):

1. **Health/timings**: respond to `GET /__opr/health` and `GET /__opr/timings`.
2. **Auth**: if `OPEN_RUNTIMES_SECRET` is set, reject requests whose
   `x-open-runtimes-secret` header doesn't match with `401`.
3. **Reserved headers**: headers prefixed `x-open-runtimes-` are control headers
   (`-secret`, `-timeout`, `-logging`, `-log-id`) and must be stripped before
   passing headers to the user's function.
4. **Build the context** and call the user's function with it:
   - `context.req` — `bodyBinary` / `bodyText` / `bodyJson` / `body`, `headers`,
     `method`, `scheme`, `host`, `port`, `path`, `query`, `queryString`, `url`.
   - `context.res` — `text(body, status?, headers?)`, `json(obj, …)`,
     `binary(bytes, …)`, `empty()`, `redirect(url, status?, headers?)`,
     `send(body, …)`.
   - `context.log(...)` / `context.error(...)` for logging.
5. **Enforced headers**: merge `OPEN_RUNTIMES_HEADERS` (JSON) over the response headers.
6. Wrap the user function in try/catch and surface errors via the logger; missing
   return → `500` with a clear message.

Functions return whatever `context.res.*` produces. Example user function:

```js
module.exports = async (context) => {
  if (context.req.headers["x-action"] === "json") {
    return context.res.json({ message: "Hello Open Runtimes 👋" });
  }
  return context.res.text("Hello Open Runtimes 👋");
};
```

## 5. Register the runtime

All runtime metadata and image build config lives in `ci/runtimes.toml`. Add a
top-level section for your runtime, then a build table, then regenerate the bake
file.

### 5.1 Runtime section

```toml
[<runtime>]
entry = "tests.<ext>"                       # default test entrypoint file
versions = ["1.83"]                          # tested versions, newest first
commands = { install = "<install cmd>", start = "bash helpers/server.sh" }
formatter = { prepare = "...", check = "...", write = "..." }
tools = "<binary> --version"                 # smoke-checked toolchain
test = "Serverless/<Name>.php"               # PHPUnit test class (see §6)
# shared = "javascript"                      # only if reusing a shared family
```

### 5.2 Build table

```toml
[<runtime>.build.versions]
"1.83" = { base = "rust:1.83-alpine3.21" }
```

Always pin the most specific base image (patch version + distro). Optional
per-version keys: `args` (Dockerfile build args), `platforms`, `version_dir`
(overlay dir override), `build_base` (php only). A `[<runtime>.build]` table can
set family-wide `platforms`, `image`, or `runtime_dir`.

Each `[<runtime>.build.versions]` key publishes the image tag
`openruntimes/<runtime>:v5-<version>`.

### 5.3 Regenerate the bake file

```bash
bun ci/bake.ts
```

CI rejects the change if `docker-bake.json` is stale or if the `versions` list
and the build version list drift apart.

## 6. Adding tests

1. Create `tests/resources/functions/<runtime>/latest/` and add a source file
   matching your `entry` (e.g. `tests.rs`) plus any manifest the language needs
   (`Cargo.toml`, `package.json`, …). The function should branch on the
   `x-action` request header and exercise dependency installation by fetching
   `https://dummyjson.com/todos/{id}` — mirror an existing runtime's test
   function closely so it passes the shared assertions.

2. Create the PHPUnit test class referenced by `test` in
   `tests/Serverless/<Name>.php`:

   ```php
   <?php

   namespace Tests\Serverless;

   use Tests\Serverless;

   class Rust extends Serverless
   {
       // most runtimes are empty and inherit every shared test
   }
   ```

3. Run the tests locally:

   ```bash
   make test ID=<runtime>-<version>      # e.g. make test ID=rust-1.83
   ```

   This builds the image, stages your fixtures, and runs the suite. See
   [docs/testing.md](testing.md) for the full local-testing guide (including
   end-to-end testing against Appwrite Community Edition and edge).

## 7. Update the README

Update the [Images table](https://github.com/open-runtimes/open-runtimes#images) in the [README](https://github.com/open-runtimes/open-runtimes/blob/main/README.md), sorted alphabetically by image.

## 8. Raise a pull request

Commit your changes and push the branch to your fork, then open a pull request
against `open-runtimes/open-runtimes`. For an initial PR, add **one version
only** — it is much easier to review.

## 🤕 Stuck?

If you need any help with the contribution, feel free to head over to our [Discord channel](https://discord.gg/fP6W2qEzfQ) and we'll be happy to help you out.
