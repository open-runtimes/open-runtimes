# How to Add a Runtime Version

This guide shows how to add a new version to an existing runtime using Bun 1.3 as an example.

## Required Changes

You need to update three components when adding a new runtime version:

### 1. Update `ci/runtimes.toml`

Add the new version to the `versions` array (newer versions first):

```diff
[bun]
entry = "tests.ts"
-versions = ["1.2", "1.1", "1.0"]
+versions = ["1.3", "1.2", "1.1", "1.0"]
# ...
```

### 2. Create Version-Specific Dockerfile

Create a new directory and Dockerfile for the version:

```bash
mkdir runtimes/bun/versions/1.3
```

Create a Dockerfile in `runtimes/bun/versions/1.3/Dockerfile`:

```dockerfile
# syntax = devthefuture/dockerfile-x:1.4.2
FROM oven/bun:1.3.0-alpine

INCLUDE ./base-before
INCLUDE ./bun
INCLUDE ./base-after
```

> You can copy `Dockerfile` contents from another version, just changing the version in image.

Always include most accurate version of image, including patch version, linux distro, and it's version to the best accuracy possible. You can search what's available on [Docker Hub](https://hub.docker.com/).

### 3. Update `.github/workflows/publish.yaml`

Add new entries to the runtime's matrix section (newer versions first):

```yaml
# Bun
- ID: bun-1.3
  RUNTIME: bun
  VERSION: "1.3"
  IMAGE: openruntimes/bun:v5-1.3
  ARCH: "linux/amd64,linux/arm64"
# ...
```

> You can copy release configuration of existing version, and just change numbers
