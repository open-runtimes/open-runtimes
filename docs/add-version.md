# How to Add a Runtime Version

This guide shows how to add a new version to an existing runtime using Bun 1.4 as an example.

## Required Changes

Adding a version is a config-only change to `ci/runtimes.toml`, plus regenerating the bake file.

### 1. Add the version to the test list

Add the new version to the `versions` array (newer versions first):

```diff
[bun]
entry = "tests.ts"
-versions = ["1.3", "1.2", "1.1", "1.0"]
+versions = ["1.4", "1.3", "1.2", "1.1", "1.0"]
# ...
```

### 2. Add the version to the build table

Add a matching entry under `[bun.build.versions]` (newer versions first):

```diff
[bun.build.versions]
+"1.4" = { base = "oven/bun:1.4.0-alpine" }
"1.3" = { base = "oven/bun:1.3.8-alpine" }
# ...
```

Always pin the most accurate base image version available, including patch version, linux distro, and its version. You can search what's available on [Docker Hub](https://hub.docker.com/).

Optional per-version keys:

| Key         | Purpose                                                                  |
|-------------|--------------------------------------------------------------------------|
| `args`      | Build args for the runtime's `runtimes/<runtime>/Dockerfile` (e.g. `PNPM_VERSION`) |
| `platforms` | Override the published platforms for this version                        |
| `version_dir` | Overlay directory under `runtimes/<runtime>/versions/` when it differs from the version name (e.g. python `ml-3.13`) |
| `build_base`  | Builder-stage base image when it differs from `base` (php only)        |

If the version needs extra files baked into the image (custom `build.gradle`, different sources), put them in `runtimes/<runtime>/versions/<version>/` — they overlay `versions/latest/` during the image build.

### 3. Regenerate docker-bake.json

```bash
bun ci/bake.ts
```

CI rejects the change if `docker-bake.json` is out of date or if the test and build version lists drift apart.

### 4. Test locally (optional)

```bash
make test ID=bun-1.4
```
