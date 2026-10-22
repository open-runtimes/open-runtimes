# Runtime security patch audit — October 2026

- **Audited:** 2026-10-22
- **Scope:** every image in `ci/runtimes.toml` `[*.build.versions]` (96 version lines), plus tool versions pinned in Dockerfiles
- **State:** §1 and §2 are implemented in the draft PR from branch `chore-security-patches-2026-10` (one commit per task). Follow-ups are tracked in [#580](https://github.com/open-runtimes/open-runtimes/issues/580) (Java/Kotlin October JDK update), [#581](https://github.com/open-runtimes/open-runtimes/issues/581) (other pending upstream releases) and [#582](https://github.com/open-runtimes/open-runtimes/issues/582) (open decisions).
- **Trigger:** a wave of CVEs in Sept–Oct 2026:
  - Node security release (Oct 21)
  - OpenJDK October CPU (Oct 20), plus the eMudhra CA compromise CVE-2026-91403 (Oct 21)
  - Go 1.26.9 and 1.26.10
  - CPython 3.14.9 and 3.13.17
  - .NET October Patch Tuesday
  - musl CVE-2026-107891
  - OpenSSL 2026-09-29 advisory (CVE-2026-84782)
- **Decided 2026-10-22** (details in §1d and §4a):
  - Move Swoole to 6.x on every PHP line that supports it (8.1–8.4)
  - Add Deno 2.9, Python 3.15 and Go 1.27
  - Add .NET 11 once it reaches GA
  - Add a floating Rust `1` line (on 1.99) alongside 1.83

The doc is organised around the work to be done:

| Section | Contents |
|---|---|
| §1 | Bumps available today, and new version lines (§1d) |
| §2 | Dockerfile and infra changes |
| §3 | Fixes we're waiting on upstream |
| §4 | Decisions to make, and the EOL lines |
| §5 | Full per-target inventory (reference) |
| §6 | Key CVEs |
| §7 | Commands to re-check |
| §8 | Caveats |

Targets use the `docker buildx bake` target names, e.g. `node-26` or `python-ml-3_13`, so each row can be built directly.

## Status legend

| Status | Meaning |
|---|---|
| `safe` | Recommended version contains every known security fix for that line |
| `needs more patches` | Line is still supported, but a known fix isn't released yet, or isn't in a Docker image yet |
| `other` | Line is EOL, frozen or unmaintained, so no more upstream fixes. Details in §4b |

## At a glance

| Status (after §1, incl. new lines) | Version lines |
|---|---|
| `safe` | 22 |
| `needs more patches` | 15 |
| `other` | 63 |
| **Total** | **100** |

The total is the original 96, plus 4 new lines (`deno-2_9`, `python-3_15`, `go-1_27`, `rust-1`). `dotnet-11` is planned but can't be built until GA, so it isn't counted.

| Work bucket | Size |
|---|---|
| §1 Bump now | 36 version lines, 5 tools, exact-tag pins for 6 Java/Kotlin targets, and 5 new lines (4 now, `dotnet-11` at GA) |
| §2 Infra changes | 8 |
| §3 Watch upstream | 14 |
| §4 Decisions | 11 (4 decided, 7 open) |

---

## 1. Bump now

All changes are in `ci/runtimes.toml` unless noted. Afterwards, run `bun ci/bake.ts` to regenerate `docker-bake.json`.

### 1a. Runtime base images

| Done | Target | Before | After | Status after | Key fixes picked up |
|---|---|---|---|---|---|
| [x] | `node-26` | `node:26.5.1-alpine3.24` | `node:26.12.1-alpine3.24` | safe | Oct 21 release: CVE-2026-111497 (H, Brotli OOB write → RCE), CVE-2026-105482/-105483/-106069 (H), 6 M/L. Bundled OpenSSL 3.5.9 |
| [x] | `node-24` | `node:24.19.0-alpine3.23` | `node:24.22.1-alpine3.24` | safe | Same October CVEs |
| [x] | `node-22` | `node:22.23.2-alpine3.23` | `node:22.23.4-alpine3.24` | safe | October CVEs incl. CVE-2026-111497 |
| [x] | `deno-2_6` | `denoland/deno:alpine-2.6.7` | `denoland/deno:alpine-2.6.10` | other | CVE-2026-27190 (H, `child_process` shell injection) |
| [x] | `bun-1_3` | `oven/bun:1.3.8-alpine` | `oven/bun:1.3.14-alpine` | other | Request smuggling ×2, tarball path traversal, response splitting, UAFs (no CVE IDs) |
| [x] | `python-3_14` | `python:3.14.6-alpine3.23` | `python:3.14.9-alpine3.24` | safe | CVE-2026-19445 (C). CVE-2026-19553/-11940/-15308 (H). Bundled pip 26.2.2, which has the certifi eMudhra fix |
| [x] | `python-3_13` | `python:3.13.14-alpine3.23` | `python:3.13.17-alpine3.24` | needs more patches | Same as 3.14, plus CVE-2026-82049 (H) |
| [x] | `python-3_12` | `python:3.12.13-alpine3.23` | `python:3.12.15-alpine3.24` | needs more patches | CVE-2026-6100 and CVE-2026-19445 (C), 7 × High |
| [x] | `python-3_11` | `python:3.11.15-alpine3.23` | `python:3.11.17-alpine3.24` | needs more patches | Same as 3.12 |
| [x] | `python-3_10` | `python:3.10.20-alpine3.23` | `python:3.10.22-alpine3.24` | other | Final release, same set as 3.12 |
| [x] | `python-ml-3_13` | `python:3.13.14-bookworm` | `python:3.13.17-bookworm` | needs more patches | Same as `python-3_13`. Base OS question: see D4 |
| [x] | `python-ml-3_12` | `python:3.12.13-bookworm` | `python:3.12.15-bookworm` | needs more patches | Same as `python-3_12` |
| [x] | `python-ml-3_11` | `python:3.11.15-bookworm` | `python:3.11.17-bookworm` | needs more patches | Same as `python-3_11` |
| [x] | `ruby-4_0` | `ruby:4.0.5-alpine3.23` | `ruby:4.0.7-alpine3.24` | safe | resolv CVE-2026-80212 (H) and CVE-2026-80213. net-imap CVE-2026-42246 (H) plus 7 more |
| [x] | `ruby-3_4` | `ruby:3.4.10-alpine3.23` | `ruby:3.4.11-alpine3.24` | safe | CVE-2026-80212 (H), CVE-2026-80213 |
| [x] | `ruby-3_3` | `ruby:3.3.10-alpine3.23` | `ruby:3.3.12-alpine3.24` | needs more patches | erb CVE-2026-41316 (H). net-imap CVE-2026-42246 (H) plus 7 more. zlib CVE-2026-27820 |
| [x] | `ruby-3_2` | `ruby:3.2.10-alpine3.23` | `ruby:3.2.11-alpine3.23` | other | CVE-2026-27820 (L) |
| [x] | `php-8_4` | `php:8.4.27-cli-alpine3.23` + Swoole `v6.1.1` | `php:8.4.27-cli-alpine3.24` + Swoole `v6.1.10` | safe | PHP was already patched. Swoole fixes: HTTP/2 UAF, WebSocket control-frame leak, Redis integer overflow (no CVE IDs) |
| [x] | `php-8_3` | `php:8.3.36-cli-alpine3.23` + Swoole `v5.1.2` | `php:8.3.36-cli-alpine3.24` + Swoole `v6.1.10` | safe | Swoole 5 → 6 (D2): CVE-2026-24814 (bundled hiredis), path-validation bypass, and the 6.1.x memory-safety fixes. `php-8_4` already runs the runtime on Swoole 6 |
| [x] | `php-8_2` | `php:8.2.35-cli-alpine3.23` + Swoole `v5.1.2` | `php:8.2.35-cli-alpine3.24` + Swoole `v6.1.10` | safe | Same as 8.3. PHP 8.2 security support ends 2026-12-31 |
| [x] | `php-8_1` | `php:8.1.34-cli-alpine3.22` + Swoole `v5.1.2` | base unchanged + Swoole `v6.1.10` | other | Swoole CVE-2026-24814 is fixed, but PHP 8.1 itself is EOL. Swoole 6.1 is the last line supporting PHP 8.1. The builder stage now installs `linux-headers`: without `linux/futex.h`, Swoole 6.1.10 compiles a fallback path that calls an undeclared `sw_usleep()` and fails. `php-8_0` stays on Swoole 4.x (Swoole 6 needs PHP ≥8.1) |
| [x] | `go-1_26` | `golang:1.26.5-alpine3.23` | `golang:1.26.10-alpine3.24` | safe | 26 CVEs, including CVE-2026-39821 (9.6), CVE-2026-78663 (9.1, HTTP/2) and CVE-2026-111943 (x509 eMudhra) |
| [x] | `go-1_25` | `golang:1.25.12-alpine3.23` | `golang:1.25.14-alpine3.24` | other | August fixes, incl. CVE-2026-39821 (9.6) |
| [x] | `dotnet-10` | `mcr.microsoft.com/dotnet/sdk:10.0.102-alpine3.23` | `mcr.microsoft.com/dotnet/sdk:10.0.402-alpine3.24` | safe | 93 CVEs, incl. CVE-2026-105016 (9.8, MsQuic RCE) and CVE-2026-88092 (9.4). The SDK band change is forced (D5) |
| [x] | `dotnet-8_0` | `mcr.microsoft.com/dotnet/sdk:8.0.417-alpine3.23` | `mcr.microsoft.com/dotnet/sdk:8.0.426-alpine3.24` | safe | 85 CVEs, incl. CVE-2026-105016 (9.8). .NET 8 reaches EOL on 2026-11-10 |
| [x] | `dart-3_13` | `dart:3.13.1` | `dart:3.13.6` | safe | SDK has bug fixes only. The image picks up Debian 13 fixes: OpenSSL (DSA-6531-1, CVE-2026-84782), curl, expat, xz, ca-certificates |
| [x] | `flutter-3_47` | `FLUTTER_VERSION=3.47.1` | `3.47.7` | safe (SDK) | libpng CVE-2026-25646, -33636, -33416. Base OS question: see D3 |
| [x] | `flutter-3_44` | `3.44.6` | `3.44.9` | other | No security content (final release) |
| [x] | `flutter-3_41` | `3.41.2` | `3.41.9` | other | Animated PNG integer overflow, Skia update |
| [x] | `flutter-3_38` | `3.38.9` | `3.38.10` | other | No security content (final release) |
| [x] | `flutter-3_32` | `3.32.6` | `3.32.8` | other | No security content (final release) |
| [x] | `flutter-3_27` | `3.27.3` | `3.27.4` | other | No security content (final release) |
| [x] | `cpp-23`, `cpp-20`, `cpp-17` | `alpine:3.23.5` | `alpine:3.23.7` | safe | musl CVE-2026-107891, OpenSSL CVE-2026-84782, zlib CVE-2026-85091, ca-certificates (eMudhra) |
| [x] | `static-1` | `joseluisq/static-web-server:2.39.0-alpine` | `joseluisq/static-web-server:2.44.1-alpine` | needs more patches | CVE-2025-67487 (symlink escape), CVE-2026-27480 (basic-auth timing), GHSA-cx3m-fg6q-xf3v (bcrypt truncation). All three affect us. ⚠ **Breaking:** needs §2 #3 |

### 1b. Pin floating Temurin tags (no version change yet)

The current tags float, which hides the October bump. Pin them now so the next bump shows up as a reviewable diff. These are not fixed yet; see §3.

| Done | Target | Before (floating) | After (exact) |
|---|---|---|---|
| [x] | `java-25`, `kotlin-2_3` | `eclipse-temurin:25-jdk-alpine-3.23` | `eclipse-temurin:25.0.4.1_1-jdk-alpine-3.23` |
| [x] | `java-21_0` | `eclipse-temurin:21-jdk-alpine-3.23` | `eclipse-temurin:21.0.12.1_1-jdk-alpine-3.23` |
| [x] | `java-17_0` | `eclipse-temurin:17-jdk-jammy` | `eclipse-temurin:17.0.20.1_1-jdk-jammy` |
| [x] | `java-11_0` | `eclipse-temurin:11-jdk-jammy` | `eclipse-temurin:11.0.32.1_1-jdk-jammy` |
| [x] | `java-8_0` | `eclipse-temurin:8-jdk-jammy` | `eclipse-temurin:8u504-b01-jdk-jammy` |
| [x] | EOL JDKs (optional) | `22-jdk-jammy`, `18-jdk-jammy`, `22-jdk-alpine`, `19-jdk-alpine`, `18-jdk-alpine` | `22.0.2_9-*`, `18.0.2.1_1-*`, `19.0.2_7-*` (all final) |

### 1c. Tools

| Done | Tool | Where | Used by | Before | After | Status | Notes |
|---|---|---|---|---|---|---|---|
| [x] | npm 12 | `NPM_VERSION` in toml | `node-26/24/22`, `deno-2_6/2_5`, `bun-1_4/1_3` | 12.0.2 | 12.2.1 | safe | Bundled tar CVE-2026-73566 (H), brace-expansion ×5 (H), undici CVE-2026-19534 (H). Engines: `^22.22.2 \|\| ^24.15.0 \|\| >=26`. See §2 #5 |
| [x] | npm 11 | `NPM_VERSION` in toml | `node-25/23/20_0` | 11.19.0 | 11.21.1 | safe | Same bundled-dependency fixes |
| [x] | Bun (SSR tooling) | `BUN_VERSION` in `runtimes/node/Dockerfile` | Node SSR images (22–26) | 1.3.2 | 1.3.14 | other | The 1.3 line is unpatched. Consider 1.4.3 |
| [x] | Drogon | `DROGON_VERSION` in `runtimes/cpp/Dockerfile` | `cpp-*` | v1.9.7 | v1.9.14 | needs more patches | Rejects CL+TE request smuggling and NUL paths. ORM SQLi still open (§3) |
| [x] | pnpm 8 | `PNPM_VERSION` in toml | `node-16_0` | `"8"` (floating) | `8.15.9` | other | Pin only. pnpm 8 and 9 are unmaintained (12 High advisories, fixed only in 10.34.5+) |

### 1d. New version lines (decided)

Process: [`docs/add-version.md`](add-version.md).
1. Add the version to the runtime's `versions` test list and its `[<runtime>.build.versions]` table, newest first.
2. Run `bun ci/bake.ts`.
3. Run `make test ID=<runtime>-<version>`.

| Done | Target | Base image | Extra changes | Status | Notes |
|---|---|---|---|---|---|
| [x] | `deno-2_9` | `denoland/deno:alpine-2.9.8` (amd64 + arm64, 2026-10-21) | `args = { NPM_VERSION = "12.2.1" }`. No overlay needed: 2.x uses `versions/latest` | safe | The only maintained Deno line. 2.9.8 adds a compile read-permission check, a napi overflow check, npm link-name validation and eMudhra CA distrust. CVE-2026-103473 is Windows-only and unfixed, so it doesn't apply. See §2 #5 |
| [x] | `python-3_15` | `python:3.15.1-alpine3.24` (amd64 + arm64, 2026-10-21) | None | safe | 3.15 is in bugfix support until 2031-10. 3.15.1 bundles pip 26.2.2, with the certifi eMudhra fix. It also includes the CVE-2026-12345 fix (backport #158431, merged 2026-10-02, before 3.15.0), which 3.11–3.13 still lack. **No `python-ml-3_15` / `python-ml-3_14`:** TensorFlow 2.21.0 still ships wheels only up to cp313 (`runtimes/python/versions/ml-3.14/NOTE.md`) |
| [x] | `go-1_27` | `golang:1.27.3-alpine3.24` (amd64 + arm64, 2026-10-22) | None | safe | 1.27.2 (Oct 8) and 1.27.3 (Oct 19) carry the same security fixes as 1.26.9 and 1.26.10, incl. CVE-2026-78663 and CVE-2026-111943. With 1.27 added, Go's two supported lines (1.27, 1.26) are both covered |
| [x] | `rust-1` (added next to `rust-1_83`) | `rust:1.99.1-alpine3.24` (amd64 + arm64) | Add `"1"` to the `versions` list and `[rust.build.versions]`, keeping `"1.83"`. `rust-version = "1.83"` stays in the Cargo manifests: it's an MSRV, so both lines build the same sources. Point the examples in `runtimes/rust/README.md` and `docs/add-runtime.md` at `v5-1` | safe | Fixes every Rust CVE since 1.83, incl. Cargo CVE-2026-55200 (C) and CVE-2026-33055 (H), on an Alpine 3.24 base. Named `1` like `node-26`: the base pin moves to each new stable, published as `openruntimes/rust:v5-1` (see D6). `rust-1_83` keeps publishing `v5-1.83`, so existing consumers don't have to switch; it stays frozen on 1.83.0 (§4b) |
| [x] | `dotnet-11` | At GA: `mcr.microsoft.com/dotnet/sdk:11.0.1xx-alpine3.24`. Today: `11.0.100-rc.2-alpine3.24` (RC2, 2026-10-13) | New overlay `runtimes/dotnet/versions/11/`, copied from `versions/8.0/` (not 10; see D11) with `<TargetFramework>net11.0</TargetFramework>`. New test fixture `tests/resources/functions/dotnet/11/`. The base carries a TODO in `runtimes.toml` | other (pre-GA) | .NET 11 is STS; GA is expected in November 2026. The PR builds it on RC2 and **must swap to the GA tag before merging** (§3 #14). Adding .NET 9 instead isn't worth it: it reaches EOL on 2026-11-10, the same day as .NET 8 |

---

## 2. Infra and Dockerfile changes

| Done | # | Change | File | Why | Affects |
|---|---|---|---|---|---|
| [x] | 1 | Add `apk upgrade --no-cache` (Alpine) or `apt-get upgrade -y` (Debian) | `docker/base-before.dockerfile` | Nothing upgrades OS packages today. Bases that are no longer rebuilt never get musl, OpenSSL or ca-certificates fixes, even when we rebuild | Every image on Alpine 3.21–3.24 or Debian 12–13, including EOL runtime lines and static-web-server. Alpine 3.20 only gets the ca-certificates 20261020 update. No effect on Alpine ≤3.19 or Debian 11 |
| [x] | 2 | Write `/etc/pip.conf` with `cert = /etc/ssl/certs/ca-certificates.crt`, then `pip install -U pip==${PIP_VERSION}` globally and inside `server-env` | `runtimes/python/Dockerfile`, `PIP_VERSION` in toml | 3.12 and older ship pip 23.0.1–25.0.1. Its vendored certifi still trusts eMudhra (GHSA-6q48-6c7w-9gw2, Critical), and pip itself has CVE-2026-3219, -6357, -8643 and -13346 | `python-*`, `python-ml-*`. The global config makes every pip, including the bundled one in the `runtime-env` venv that user builds create, use the OS trust store, which #1 keeps current. `PIP_VERSION` defaults to 26.2.2. Python 3.9 gets 26.0.1 and 3.8 gets 25.0.1, the last releases they support (pip ≥26.1 needs 3.10) |
| [x] | 3 | `USER root` before the `apk add` steps | `runtimes/static/Dockerfile` | static-web-server ≥2.40 runs as `sws:sws` with WORKDIR `/home/sws` | `static-1`. Required for the §1a bump |
| [x] | 4 | Exact Temurin tags | `ci/runtimes.toml` | See §1b | `java-*`, `kotlin-*` |
| [x] | 5 | Check that the Alpine `nodejs` / `nodejs-current` package satisfies the npm 12.2.1 engines | `runtimes/deno/Dockerfile`, `runtimes/bun/Dockerfile` | npm 12.2.1 requires Node `^22.22.2 \|\| ^24.15.0 \|\| >=26`. Verified: engines are identical to 12.0.2 (and 11.21.1 to 11.19.0), so nothing changes | `deno-2_6/2_5`, `bun-1_4/1_3` |
| [ ] | 6 | *Optional:* remove the emSign ECC Root CA - G3 alias from `cacerts` until the Temurin October builds ship | `runtimes/java/Dockerfile`, `runtimes/kotlin/Dockerfile` | Interim mitigation for CVE-2026-91403 (9.1) | `java-*`, `kotlin-*` |
| [ ] | 7 | *Optional:* `gem install resolv -v 0.3.2` | `ruby-3_3` version overlay | Workaround for CVE-2026-80212 (H) until 3.3.13 | `ruby-3_3` |
| [x] | 8 | Regenerate and verify the bake file | `bun ci/bake.ts && bun ci/bake.ts --check` | `docker-bake.json` is generated from the toml | all |

---

## 3. Watch list (waiting on upstream)

Rows 1–2 are tracked in [#580](https://github.com/open-runtimes/open-runtimes/issues/580) and rows 3–14 in [#581](https://github.com/open-runtimes/open-runtimes/issues/581).

| # | Affects | Waiting for | Then | Where to check |
|---|---|---|---|---|
| 1 | `java-25/21_0/17_0/11_0/8_0`, `kotlin-2_3` | Temurin October CPU images `25.0.5_8`, `21.0.13_*`, `17.0.21_*`, `11.0.33_*`, `8u512-*` (amd64 + arm64). As of 10-22 only the x64 25.0.5+8 binary exists, with no Docker image | Bump the §1b pins | https://api.adoptium.net/v3/info/release_versions, Docker Hub `eclipse-temurin` tags |
| 2 | Same as #1 | CVE-2026-91403 respin. Upstream bumped to 25.0.5.0.2, 21.0.13.0.2 and 17.0.21.0.2; Temurin may ship these or rely on its cacerts removal (temurin-build #4588) | Bump again if respun | https://openjdk.org/groups/vulnerability/advisories/2026-10-21 |
| 3 | `bun-1_4` | Bun 1.4.4 with the TLS fixes from PR #44618 (+#45124, #45139, #45422): client-cert disclosure, plain TCP when `tls:` is set, mTLS `crl` ignored | Bump | https://github.com/oven-sh/bun/releases |
| 4 | `python-3_13`, `python-ml-3_13` | 3.13.18 with the CVE-2026-12345 backport (PR #158429) | Bump | https://www.python.org/downloads/ |
| 5 | `python-3_12/3_11`, `python-ml-3_12/3_11` | CVE-2026-12345 (3.12 PR #159074 open; no 3.11 PR), CVE-2026-3446, CVE-2026-6019 (no backport PRs) | Bump | https://devguide.python.org/versions/ |
| 6 | `ruby-3_3` | 3.3.13 with resolv 0.3.2 (already committed as 619ea0f602) | Bump, drop §2 #7 | https://www.ruby-lang.org/en/news/ |
| 7 | `ruby-4_0` | `ruby:4.0.8-alpine3.24` tag. Non-security; UAF fixes | Bump | Docker Hub `ruby` tags |
| 8 | `node-26`, `node-24` | 26.12.2 / 24.22.2 reverting regression #66906 (webstreams lose AsyncLocalStorage context). Non-security | Bump if SSR frameworks are affected | nodejs/node #66913, #66914 |
| 9 | `php-8_4/8_3/8_2/8_1` | Swoole 6.1.11 with unreleased hardening: oversized Upgrade values, EOF packet limits, SOCKS5 lengths, compressed WebSocket control frames | Bump | https://github.com/swoole/swoole-src/compare/v6.1.10...6.1 |
| 10 | `flutter-*`, `python-ml-*` | Debian 12 DLA for OpenSSL CVE-2026-84782 (H) and curl CVE-2026-8286, or move off bookworm (D3/D4) | Rebuild | https://security-tracker.debian.org/tracker/CVE-2026-84782 |
| 11 | `static-1` | Upstream image rebuilt on Alpine ≥3.23.7 (or do §2 #1) | Bump | Docker Hub `joseluisq/static-web-server` |
| 12 | `cpp-*` | Drogon fix for ORM SQLi CVE-2026-94143 / -94144 (issues #2575, #2576). Low exposure: no DB client libs in the image | Bump | https://github.com/drogonframework/drogon/releases |
| 13 | `node-21_0/18_0`, `deno-2_0/1_46/1_40`, `bun-1_2/1_1/1_0` | npm 10.x release with a brace-expansion fix (bundled 2.0.2 has 5 × High) | Bump `NPM_VERSION` | `curl -s https://registry.npmjs.org/npm \| jq '."dist-tags"'` |
| 14 | `dotnet-11` (new, §1d) | .NET 11 GA (currently `11.0.0-rc.2`, support phase "go-live"). Expected November 2026 | Add the line on the GA SDK tag | `curl -s https://builds.dotnet.microsoft.com/dotnet/release-metadata/releases-index.json \| jq '."releases-index"[0]'` |

---

## 4. Decisions needed

### 4a. Structural decisions

Open items are tracked in [#582](https://github.com/open-runtimes/open-runtimes/issues/582).

| # | Decision | Status | Outcome / options | Recommendation |
|---|---|---|---|---|
| D1 | Alpine 3.23 → 3.24 for bumped lines | Open | Keep `-alpine3.23` tags where they're published, or move to `-alpine3.24` | `-alpine3.24`. Supported to 2028-06, already used by `node-26`, and forced for .NET and the new lines. 3.23 is supported to 2027-11, so staying is also acceptable (`-alpine3.23` availability was only verified for Node and Python) |
| D2 | Swoole for PHP 8.1–8.3 | **Decided:** Swoole 6 | `v6.1.10` for `php-8_4/8_3/8_2/8_1` (Swoole 6.1 supports PHP 8.1–8.4). `php-8_0` stays on 4.x | — |
| D3 | Flutter base OS | Open | `debian:12.15-slim` (LTS-only, OpenSSL High open), or `debian:13.7-slim` | `debian:13.7-slim` |
| D4 | Python ML base OS | Open | `-bookworm` or `-trixie` (the default variant now) | `-trixie` |
| D5 | .NET 10 SDK band | Forced | 10.0.402 (4xx) is the only patched image. The 1xx band stops at 10.0.103 and `alpine3.23` was dropped | 10.0.402 |
| D6 | Rust | **Decided:** add a floating `1` line, keep 1.83 | `rust-1` (currently 1.99.1) added next to `rust-1_83` (§1d). Rust ships a 1.x minor every 6 weeks under its backward-compatibility promise, editions are opt-in per crate, and only the latest stable gets fixes, so per-minor lines would be frozen and unpatched by the time they publish. The `1` line follows the latest stable like `node-26` follows Node 26. 1.83 stays published for existing consumers | — |
| D7 | Add newer supported lines | **Decided** (partly) | Adding Deno 2.9, Python 3.15, Go 1.27, and .NET 11 at GA (§1d). Still open: PHP 8.5 (8.5.12; needs Swoole 6.2.x, which supports PHP 8.2–8.5), Swift 6.3 / 6.4 | Revisit PHP 8.5 together with Swoole 6.2 |
| D8 | .NET 8 EOL on 2026-11-10 | Open | — | Plan the deprecation notice; `dotnet-11` lands around the same time |
| D9 | Kotlin on EOL JDKs (22 / 19 / 18) | Open | Keep, or move to a JDK 21 LTS base (compiler compatibility untested) | Test `kotlin-2_0` on Temurin 21 |
| D10 | Policy for EOL lines in §4b | Open | Publish as-is, rebuild with §2 #1 (now done for all), or deprecate | Consider deprecating anything on Alpine ≤3.20 or Debian 11, where §2 #1 can't help |
| D11 | Stale `dotnet-10` overlay (found while adding .NET 11) | Open | `runtimes/dotnet/versions/10/Program.cs` predates the safe-entrypoint (753a7f13) and env-config-at-startup (dc4a3bca) changes that 6.0–8.0 have | Sync it with 8.0 in a separate PR. `dotnet-11` is already based on 8.0 |

### 4b. EOL / frozen lines (status `other`)

| Runtime | Lines | EOL / last release | Worst known unfixed | Notes |
|---|---|---|---|---|
| Node | 25 | EOL 2026-06-01 | CVE-2026-111497, -48933, -56848 (H) | No alpine3.24 tag |
| Node | 23, 21.0, 20.0, 19.0, 18.0, 16.0 | EOL 2025-06 / 2024-06 / 2026-04-30 / 2023-06 / 2025-04-30 / 2023-09 | Every 2025–26 Node High | 21/18 are on Alpine 3.20 (EOL). 19/16 are on 3.18 (EOL). 16 bundles OpenSSL 1.1.1 |
| Deno | 2.6 | Unmaintained (only 2.9 is) | CVE-2026-44726, -49440 (H) | Bumped to final 2.6.10 in §1a |
| Deno | 2.5 | LTS ended 2026-04-30 | CVE-2026-27190 (H) | — |
| Deno | 2.0, 1.46 | Final | CVE-2026-22863 (C 9.2), CVE-2025-21620 (H) | — |
| Deno | 1.40, 1.35, 1.24, 1.21 | Final | Additionally CVE-2024-27936, CVE-2024-34346, CVE-2023-28446 (H) | amd64 only |
| Bun | 1.3 | No release since 1.4.0 (2026-08-20) | CVE-2026-48618, HTTP/2 rapid reset | Not backported |
| Bun | 1.2, 1.1, 1.0 | Final | Everything fixed in 1.3.x / 1.4.x; 1.0 also CVE-2024-21548 | — |
| Python | 3.10 | EOL 2026-10-01 | CVE-2026-12345, -3446, -6019 | docker-library stopped building it (PR #1133) |
| Python | 3.9, 3.8 | EOL 2025-10-31 / 2024-10-07 | CVE-2026-19445, -6100 (C) | 3.8 is on Alpine 3.20 (EOL) |
| Ruby | 3.2 | EOL 2026-04-01 | CVE-2026-41316, -80212, -42246 (H) | Docker stopped building it |
| Ruby | 3.1, 3.0 | EOL 2025-03 / 2024-04 | All of the above, plus REXML and others | 3.1 is on Alpine 3.20, 3.0 on 3.16 (both EOL) |
| PHP | 8.1, 8.0 | EOL 2025-12-31 / 2023-11 | CVE-2026-109618 / -109619 (C, probable) | 8.1 moves to Swoole 6 (D2). 8.0 stays on Swoole 4.x (EOL, CVE-2026-24814) and Alpine 3.16 |
| Go | 1.25, 1.24, 1.23 | EOL 2026-08-19 / 2026-02-10 / 2025-08-12 | CVE-2026-78663 (9.1), CVE-2026-111943 | Supported lines 1.26 and 1.27 are covered (`go-1_27` added, D7) |
| Java | 22, 18.0 | EOL | CVE-2026-91403 (9.1), plus every CPU since 2024-10 / 2022-10 | — |
| Kotlin | 2.0 (JDK 22), 1.9 / 1.8 (JDK 19), 1.6 (JDK 18) | EOL JDKs | Same as Java EOL | See D9 |
| .NET | 7.0, 6.0 | EOL 2024-05 / 2024-11 | CVE-2025-55315 (9.9), CVE-2026-105016 (9.8), inferred | Alpine 3.19 / 3.20 are EOL |
| Swift | 5.10, 5.9, 5.8 | Final; images no longer rebuilt | No toolchain CVEs | Ubuntu packages are stale (fix: §2 #1) |
| Dart | 3.12, 3.11 | Frozen | Missing the Sept/Oct Debian DSAs | Fix: §2 #1 |
| Dart | 3.10, 3.9, 3.8, 3.5, 3.3, 3.1 | Frozen | CVE-2026-27704 (L, pub zip-slip) | — |
| Dart | 3.0, 2.19, 2.18, 2.17, 2.16 | Frozen; Debian 11 LTS ended 2026-08-31 | CVE-2026-27704. 2.17 / 2.16 also CVE-2022-3095 | `apt-get update` will break once bullseye moves to archive.debian.org |
| Rust | 1.83 | Final (Rust only patches the latest stable) | Cargo CVE-2026-55200 (C), CVE-2026-33055 (H) | Kept for existing consumers (D6). On Alpine 3.21, EOL 2026-11-01; OS packages come from §2 #1 until then |
| Flutter | 3.44, 3.41, 3.38, 3.35, 3.32, 3.29, 3.27, 3.24 | Only 3.47 gets hotfixes | libpng CVE-2026-25646 / -33636 / -33416. ≤3.38 also has CVE-2026-27704 | — |

---

## 5. Full inventory (reference)

Each target's state after the recommended bumps. `=` means no change, and `—` in the Before column marks a new line.

| Target | Before | After | Status | See |
|---|---|---|---|---|
| `node-26` | node:26.5.1-alpine3.24 | node:26.12.1-alpine3.24 | safe | §1a, §3 #8 |
| `node-25` | node:25.9.0-alpine3.23 | = | other | §4b |
| `node-24` | node:24.19.0-alpine3.23 | node:24.22.1-alpine3.24 | safe | §1a, §3 #8 |
| `node-23` | node:23.11.1-alpine3.22 | = | other | §4b |
| `node-22` | node:22.23.2-alpine3.23 | node:22.23.4-alpine3.24 | safe | §1a |
| `node-21_0` | node:21.7.3-alpine3.20 | = | other | §4b |
| `node-20_0` | node:20.20.2-alpine3.23 | = | other | §4b |
| `node-19_0` | node:19.9.0-alpine3.18 | = | other | §4b |
| `node-18_0` | node:18.20.8-alpine3.20 | = | other | §4b |
| `node-16_0` | node:16.20.2-alpine3.18 | = | other | §4b |
| `deno-2_9` | — | denoland/deno:alpine-2.9.8 | safe | §1d (new) |
| `deno-2_6` | denoland/deno:alpine-2.6.7 | alpine-2.6.10 | other | §1a, §4b |
| `deno-2_5` | denoland/deno:alpine-2.5.7 | = | other | §4b |
| `deno-2_0` | denoland/deno:alpine-2.0.6 | = | other | §4b |
| `deno-1_46` | denoland/deno:alpine-1.46.3 | = | other | §4b |
| `deno-1_40` | denoland/deno:alpine-1.40.5 | = | other | §4b |
| `deno-1_35` | denoland/deno:alpine-1.35.3 | = | other | §4b |
| `deno-1_24` | denoland/deno:alpine-1.24.3 | = | other | §4b |
| `deno-1_21` | denoland/deno:alpine-1.21.3 | = | other | §4b |
| `bun-1_4` | oven/bun:1.4.3-alpine | = | needs more patches | §3 #3 |
| `bun-1_3` | oven/bun:1.3.8-alpine | 1.3.14-alpine | other | §1a, §4b |
| `bun-1_2` | oven/bun:1.2.23-alpine | = | other | §4b |
| `bun-1_1` | oven/bun:1.1.45-alpine | = | other | §4b |
| `bun-1_0` | oven/bun:1.0.36-alpine | = | other | §4b |
| `python-3_15` | — | python:3.15.1-alpine3.24 | safe | §1d (new) |
| `python-3_14` | python:3.14.6-alpine3.23 | python:3.14.9-alpine3.24 | safe | §1a |
| `python-3_13` | python:3.13.14-alpine3.23 | python:3.13.17-alpine3.24 | needs more patches | §1a, §3 #4 |
| `python-3_12` | python:3.12.13-alpine3.23 | python:3.12.15-alpine3.24 | needs more patches | §1a, §2 #2, §3 #5 |
| `python-3_11` | python:3.11.15-alpine3.23 | python:3.11.17-alpine3.24 | needs more patches | §1a, §2 #2, §3 #5 |
| `python-3_10` | python:3.10.20-alpine3.23 | python:3.10.22-alpine3.24 | other | §1a, §4b |
| `python-3_9` | python:3.9.25-alpine3.22 | = | other | §4b |
| `python-3_8` | python:3.8.20-alpine3.20 | = | other | §4b |
| `python-ml-3_13` | python:3.13.14-bookworm | python:3.13.17-bookworm | needs more patches | §1a, §3 #4, D4 |
| `python-ml-3_12` | python:3.12.13-bookworm | python:3.12.15-bookworm | needs more patches | §1a, §3 #5, D4 |
| `python-ml-3_11` | python:3.11.15-bookworm | python:3.11.17-bookworm | needs more patches | §1a, §3 #5, D4 |
| `dart-3_13` | dart:3.13.1 | dart:3.13.6 | safe | §1a |
| `dart-3_12` | dart:3.12.2 | = | other | §4b |
| `dart-3_11` | dart:3.11.6 | = | other | §4b |
| `dart-3_10` | dart:3.10.9 | = | other | §4b |
| `dart-3_9` | dart:3.9.4 | = | other | §4b |
| `dart-3_8` | dart:3.8.3 | = | other | §4b |
| `dart-3_5` | dart:3.5.4 | = | other | §4b |
| `dart-3_3` | dart:3.3.4 | = | other | §4b |
| `dart-3_1` | dart:3.1.5 | = | other | §4b |
| `dart-3_0` | dart:3.0.7 | = | other | §4b |
| `dart-2_19` | dart:2.19.6 | = | other | §4b |
| `dart-2_18` | dart:2.18.7 | = | other | §4b |
| `dart-2_17` | dart:2.17.7 | = | other | §4b |
| `dart-2_16` | dart:2.16.2 | = | other | §4b |
| `flutter-3_47` | 3.47.1 on debian:12.15-slim | 3.47.7 | safe (SDK) | §1a, §3 #10, D3 |
| `flutter-3_44` | 3.44.6 | 3.44.9 | other | §1a, §4b |
| `flutter-3_41` | 3.41.2 | 3.41.9 | other | §1a, §4b |
| `flutter-3_38` | 3.38.9 | 3.38.10 | other | §1a, §4b |
| `flutter-3_35` | 3.35.7 | = | other | §4b |
| `flutter-3_32` | 3.32.6 | 3.32.8 | other | §1a, §4b |
| `flutter-3_29` | 3.29.3 | = | other | §4b |
| `flutter-3_27` | 3.27.3 | 3.27.4 | other | §1a, §4b |
| `flutter-3_24` | 3.24.5 | = | other | §4b |
| `ruby-4_0` | ruby:4.0.5-alpine3.23 | ruby:4.0.7-alpine3.24 | safe | §1a, §3 #7 |
| `ruby-3_4` | ruby:3.4.10-alpine3.23 | ruby:3.4.11-alpine3.24 | safe | §1a |
| `ruby-3_3` | ruby:3.3.10-alpine3.23 | ruby:3.3.12-alpine3.24 | needs more patches | §1a, §2 #7, §3 #6 |
| `ruby-3_2` | ruby:3.2.10-alpine3.23 | ruby:3.2.11-alpine3.23 | other | §1a, §4b |
| `ruby-3_1` | ruby:3.1.7-alpine3.20 | = | other | §4b |
| `ruby-3_0` | ruby:3.0.7-alpine3.16 | = | other | §4b |
| `php-8_4` | php:8.4.27-cli-alpine3.23 + swoole v6.1.1 | php:8.4.27-cli-alpine3.24 + v6.1.10 | safe | §1a, §3 #9 |
| `php-8_3` | php:8.3.36-cli-alpine3.23 + swoole v5.1.2 | php:8.3.36-cli-alpine3.24 + v6.1.10 | safe | §1a, D2, §3 #9 |
| `php-8_2` | php:8.2.35-cli-alpine3.23 + swoole v5.1.2 | php:8.2.35-cli-alpine3.24 + v6.1.10 | safe | §1a, D2, §3 #9 |
| `php-8_1` | php:8.1.34-cli-alpine3.22 + swoole v5.1.2 | = + v6.1.10 | other | §1a, D2, §4b |
| `php-8_0` | php:8.0.30-cli-alpine3.16 + swoole v4.7.0 | = | other | §4b |
| `swift-6_2` | swift:6.2.4-jammy | = | safe | — |
| `swift-5_10` | swift:5.10.1-jammy | = | other | §4b |
| `swift-5_9` | swift:5.9.2-jammy | = | other | §4b |
| `swift-5_8` | swift:5.8.1-jammy | = | other | §4b |
| `kotlin-2_3` | eclipse-temurin:25-jdk-alpine-3.23 | pin 25.0.4.1_1, then 25.0.5 | needs more patches | §1b, §3 #1–2 |
| `kotlin-2_0` | eclipse-temurin:22-jdk-alpine | = | other | §4b, D9 |
| `kotlin-1_9` | eclipse-temurin:19-jdk-alpine | = | other | §4b, D9 |
| `kotlin-1_8` | eclipse-temurin:19-jdk-alpine | = | other | §4b, D9 |
| `kotlin-1_6` | eclipse-temurin:18-jdk-alpine | = | other | §4b, D9 |
| `java-25` | eclipse-temurin:25-jdk-alpine-3.23 | pin 25.0.4.1_1, then 25.0.5 | needs more patches | §1b, §3 #1–2 |
| `java-22` | eclipse-temurin:22-jdk-jammy | = | other | §4b |
| `java-21_0` | eclipse-temurin:21-jdk-alpine-3.23 | pin 21.0.12.1_1, then 21.0.13 | needs more patches | §1b, §3 #1–2 |
| `java-18_0` | eclipse-temurin:18-jdk-jammy | = | other | §4b |
| `java-17_0` | eclipse-temurin:17-jdk-jammy | pin 17.0.20.1_1, then 17.0.21 | needs more patches | §1b, §3 #1–2 |
| `java-11_0` | eclipse-temurin:11-jdk-jammy | pin 11.0.32.1_1, then 11.0.33 | needs more patches | §1b, §3 #1–2 |
| `java-8_0` | eclipse-temurin:8-jdk-jammy | pin 8u504-b01, then 8u512 | needs more patches | §1b, §3 #1–2 |
| `cpp-23` | alpine:3.23.5 | alpine:3.23.7 | safe | §1a |
| `cpp-20` | alpine:3.23.5 | alpine:3.23.7 | safe | §1a |
| `cpp-17` | alpine:3.23.5 | alpine:3.23.7 | safe | §1a |
| `dotnet-11` | — | sdk:11.0.1xx-alpine3.24 at GA (RC2 today) | other (pre-GA, not counted) | §1d (new), §3 #14 |
| `dotnet-10` | sdk:10.0.102-alpine3.23 | sdk:10.0.402-alpine3.24 | safe | §1a, D5 |
| `dotnet-8_0` | sdk:8.0.417-alpine3.23 | sdk:8.0.426-alpine3.24 | safe | §1a, D8 |
| `dotnet-7_0` | sdk:7.0.410-alpine3.19 | = | other | §4b |
| `dotnet-6_0` | sdk:6.0.428-1-alpine3.20 | = | other | §4b |
| `go-1_27` | — | golang:1.27.3-alpine3.24 | safe | §1d (new) |
| `go-1_26` | golang:1.26.5-alpine3.23 | golang:1.26.10-alpine3.24 | safe | §1a |
| `go-1_25` | golang:1.25.12-alpine3.23 | golang:1.25.14-alpine3.24 | other | §1a, D7 |
| `go-1_24` | golang:1.24.13-alpine3.23 | = | other | §4b |
| `go-1_23` | golang:1.23.12-alpine3.22 | = | other | §4b |
| `rust-1` | — | rust:1.99.1-alpine3.24 | safe | §1d (new), D6 |
| `rust-1_83` | rust:1.83-alpine | = | other | §4b, D6 |
| `static-1` | joseluisq/static-web-server:2.39.0-alpine | 2.44.1-alpine | needs more patches | §1a, §2 #3, §3 #11 |

---

## 6. Key CVEs

| CVE | Component | Severity | Fixed in | Source |
|---|---|---|---|---|
| CVE-2026-111497 | Node (Brotli OOB write → RCE) | High | 26.12.1, 24.22.1, 22.23.4 | [Node Oct 2026](https://nodejs.org/en/blog/vulnerability/october-2026-security-releases) |
| CVE-2026-105482, -105483, -106069 | Node | High | Same as above (-105483 not in 22) | Same as above |
| CVE-2026-91403 | OpenJDK (compromised eMudhra intermediates) | 9.1 | Pending (see §3 #2) | [OpenJDK 2026-10-21](https://openjdk.org/groups/vulnerability/advisories/2026-10-21) |
| CVE-2026-91415 (+39 more) | OpenJDK October CPU | 7.0 (top) | 25.0.5, 21.0.13, 17.0.21, 11.0.33, 8u512 | [OpenJDK 2026-10-20](https://openjdk.org/groups/vulnerability/advisories/2026-10-20) |
| CVE-2026-39821 | Go idna | 9.6 | 1.26.6, 1.25.13 | [Go releases](https://go.dev/doc/devel/release) |
| CVE-2026-78663 | Go net/http HTTP/2 | 9.1 | 1.26.9 | Same as above |
| CVE-2026-111943 | Go crypto/x509 (eMudhra) | n/a | 1.26.10 | Same as above |
| CVE-2026-19445 | CPython | Critical | 3.14.9, 3.13.17, 3.12.15, 3.11.17, 3.10.22 | [security-announce](https://mail.python.org/archives/list/security-announce@python.org/) |
| CVE-2026-6100 | CPython | Critical | 3.12.15, 3.11.17, 3.10.22 (and earlier in 3.13 / 3.14) | Same as above |
| CVE-2026-12345 | CPython tempfile race (gh-157579) | Medium | main, 3.15.0+, 3.14.9. The 3.13 and 3.12 backports are open. The CVE record wrongly lists 3.12.15 | Same as above |
| GHSA-6q48-6c7w-9gw2 | certifi (eMudhra) | Critical | certifi 2026.10.19, pip 26.2.2 | Same as above |
| CVE-2026-80212 | Ruby resolv | High 7.5 | 4.0.7, 3.4.11. 3.3 pending | [ruby-lang.org](https://www.ruby-lang.org/en/news/2026/08/27/multiple-vulnerabilities-in-resolv/) |
| CVE-2026-41316 | Ruby erb | High 8.1 | 3.3.12 | [ruby-lang.org](https://www.ruby-lang.org/en/news/2026/04/21/erb-cve-2026-41316/) |
| CVE-2026-109618, -109619 | PHP | Critical (RCE) | 8.4.27, 8.3.36, 8.2.35 | [PHP ChangeLog](https://www.php.net/ChangeLog-8.php) |
| CVE-2026-24814 | Swoole bundled hiredis | Critical per CNA (likely overstated) | Swoole 6.1.x only (adopted for PHP 8.1–8.4, D2). Still open on `php-8_0` (Swoole 4.x) | [NVD](https://nvd.nist.gov/vuln/detail/CVE-2026-24814) |
| CVE-2026-105016 | .NET MsQuic RCE | 9.8 | 10.0.13, 8.0.32 | [.NET 10.0.13 notes](https://github.com/dotnet/core/blob/main/release-notes/10.0/10.0.13/10.0.13.md) |
| CVE-2026-27190 | Deno `child_process` injection | High 8.1 | 2.6.8 | [GHSA-hmh4-3xvx-q5hr](https://github.com/denoland/deno/security/advisories/GHSA-hmh4-3xvx-q5hr) |
| CVE-2026-22863 | Deno | Critical 9.2 | 2.6.0 (2.5.7 backport likely, unconfirmed) | [GHSA-5379-f5hf-w38v](https://github.com/denoland/deno/security/advisories/GHSA-5379-f5hf-w38v) |
| CVE-2026-107891 | musl regcomp heap OOB write | — | Alpine 3.21.9–3.24.3 | [openwall](https://www.openwall.com/lists/musl/2026/10/21/1) |
| CVE-2026-84782 | OpenSSL DTLS | High | OpenSSL 3.5.9, Alpine 3.23.7, Debian 13 DSA-6531-1. Unfixed in Debian 12 | [OpenSSL advisory](https://openssl-library.org/news/secadv/20260929.txt) |
| CVE-2026-55200 | libssh2 in Cargo | Critical (CVSS4 9.2) | Rust 1.96.1 | [Rust blog](https://blog.rust-lang.org/2026/06/30/Rust-1.96.1/) |
| CVE-2025-67487, CVE-2026-27480 | static-web-server | — | ≤ 2.44.1 | [SWS advisories](https://github.com/static-web-server/static-web-server/security/advisories) |
| CVE-2026-25646, -33636, -33416 | libpng in the Flutter engine | — | Flutter 3.47.2 | [flutter#190987](https://github.com/flutter/flutter/issues/190987) |
| CVE-2026-73566 | tar (bundled in npm) | High | npm 12.2.1, 11.21.1 | [GitHub advisories](https://github.com/advisories?query=CVE-2026-73566) |

---

## 7. Re-checking

```sh
# Docker Hub: exact tag + architectures (swap repo/name filter; paginate via .next)
curl -s "https://hub.docker.com/v2/repositories/library/node/tags?page_size=100&name=24.22" \
  | jq -r '.results[] | "\(.name) \(.last_updated) \([.images[].architecture]|join(","))"'

# MCR (.NET)
curl -s https://mcr.microsoft.com/v2/dotnet/sdk/tags/list | jq -r '.tags[]' | grep -E '^(10|8)\.0\..*alpine3\.24$'

# Temurin releases
curl -s "https://api.adoptium.net/v3/info/release_versions?release_type=ga&page_size=20&sort_order=DESC" | jq -r '.versions[].openjdk_version'

# After editing ci/runtimes.toml
bun ci/bake.ts && bun ci/bake.ts --check
docker buildx bake node-26 --load --set '*.platform=linux/amd64'
```

Primary sources:

| Runtime | Sources |
|---|---|
| Node | [Releases](https://github.com/nodejs/node/releases), [security posts](https://nodejs.org/en/blog/vulnerability) |
| Deno | [Releases](https://github.com/denoland/deno/releases), [advisories](https://github.com/denoland/deno/security/advisories), [LTS](https://docs.deno.com/runtime/fundamentals/stability_and_releases/) |
| Bun | [Releases](https://github.com/oven-sh/bun/releases), [blog](https://bun.com/blog) |
| Python | [Downloads](https://www.python.org/downloads/), [versions](https://devguide.python.org/versions/) |
| Ruby | [Releases](https://www.ruby-lang.org/en/downloads/releases/), [security](https://www.ruby-lang.org/en/security/) |
| PHP | [Releases](https://www.php.net/releases/), [supported versions](https://www.php.net/supported-versions.php) |
| Swoole | [Releases](https://github.com/swoole/swoole-src/releases) |
| Go | [Release history](https://go.dev/doc/devel/release), [vuln DB](https://vuln.go.dev) |
| Java | [Adoptium](https://adoptium.net/temurin/releases/), [Oracle CPU Oct 2026](https://www.oracle.com/security-alerts/cpuoct2026.html) |
| .NET | [Releases index](https://builds.dotnet.microsoft.com/dotnet/release-metadata/releases-index.json) |
| Swift | [Releases](https://github.com/swiftlang/swift/releases) |
| Dart | [Archive](https://dart.dev/get-dart/archive) |
| Flutter | [Releases JSON](https://storage.googleapis.com/flutter_infra_release/releases/releases_linux.json) |
| Alpine | [releases.json](https://alpinelinux.org/releases.json) |
| Debian | [LTS](https://wiki.debian.org/LTS), [security tracker](https://security-tracker.debian.org/tracker/) |

## 8. Caveats

- Testing so far:
  - Local `linux/arm64` image builds (build only, no PHPUnit) pass for `php-8_1`, `php-8_3`, `static-1`, `python-3_15`, `python-3_8`, `rust-1_99` (now `rust-1`), `dotnet-11`, `ruby-3_0`, `dart-2_16`, `go-1_27`, `deno-2_9` and `cpp-23`.
  - All 89 base images exist for the platforms they're built for.
  - The full test matrix runs in CI on the PR.
- The new `apt-get upgrade` is a no-op on Debian 11 bases (Dart 2.16–3.0). Their stale package index points at security packages that now 404. That's non-fatal, but those images stay unpatched.
- Python 3.15.1: the CVE-2026-12345 fix and pip 26.2.2 are verified. The other October CPython fixes (e.g. CVE-2026-19445) are assumed to be included because 3.15.1 shipped the same day as 3.14.9, but weren't checked one by one.
- The .NET 11 GA date (November 2026) is the usual .NET Conf timing, not a confirmed date.
- The October Node CVE IDs aren't on cve.org yet. They match between the Node blog and the GitHub release notes.
- The "unfixed" lists for EOL lines are inferred from vendor policy ("EOL lines are affected"), not confirmed per version.
- Bun and Swoole security fixes have no CVE IDs, so their severity is unknown.
- `-alpine3.23` tags for the new patch versions were only verified for Node. See D1.
- The Debian base of some old Dart images (3.3, 3.10–3.12, 2.17–2.18) is inferred from dart-docker history.
