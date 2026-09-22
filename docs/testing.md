# Testing Open Runtimes locally

There are two distinct things you can test, and they use different loops:

1. **The runtime itself** — does a runtime image build correctly and serve
   functions the way the test suite expects? This is the **in-repo harness**
   (`make test`). It is fast, deterministic, and is what CI runs. Use it for
   almost all runtime work.
2. **End-to-end through Appwrite Community Edition** — does a freshly-built
   runtime image actually run under a real executor inside a self-hosted
   [Appwrite Community Edition](https://github.com/appwrite/appwrite) install
   (or [edge](https://github.com/appwrite/edge))? This is a heavier integration
   loop. Use it only to smoke-test image packaging against a real executor.

Start with loop 1. Only promote to loop 2 once the in-repo harness is green.

## Prerequisites

- Docker (with `buildx`)
- [Bun](https://bun.sh) — drives the test orchestration (`brew install oven-sh/bun/bun`)
- PHP dependencies installed once: `docker run --rm -v $PWD:/app composer install`

## Loop 1 — the in-repo harness

Run the full cycle for a single runtime/version:

```bash
make test ID=node-25        # build image, stage fixtures, run the PHP suite
make test ID=node           # latest version of a runtime
make test ID=next-js        # SSR alias (enforced runtime)
```

Under the hood this is `bun ci/test.ts <id>`, which:

1. builds the image with `docker buildx bake` (tagged `open-runtimes/test-runtime`),
2. runs the formatter check (latest version only),
3. stages fixtures from `tests/resources/functions/<runtime>/` into `tests/.runtime/`,
4. runs `tools` → `build` (plus variant builds) → the `serve` trio,
5. runs PHPUnit against the running servers via `tests/compose.yaml`.

Useful flags (pass through `bun ci/test.ts` directly):

| Command | Effect |
|---|---|
| `bun ci/test.ts node-25 --skip-image` | reuse the existing `open-runtimes/test-runtime` image |
| `bun ci/test.ts node-25 --image-only` | build the image and stop |
| `make image ID=node-25` | alias for `--image-only` |
| `make format ID=node` | run formatters in write mode |
| `make clean` | tear down test containers and staged fixtures |

**A run passes if** every step exits `0` and PHPUnit reports green. A failure
here is almost always a build/packaging or hook regression — fix it before
touching Community Edition or edge.

To sweep several runtimes:

```bash
for id in node-25 python-3.14 swift-6.2 rust-1.83 static-1; do
  make test ID=$id || echo "FAIL: $id"
done
```

## Loop 2 — end-to-end through Appwrite Community Edition (docker-compose)

[Appwrite Community Edition](https://github.com/appwrite/appwrite) is the
open-source, self-hostable build of Appwrite, shipped as a set of Docker
microservices in a single `docker-compose.yml`. One of those services is the
`openruntimes-executor` — the same executor that builds and runs these runtime
images in production — which makes it a realistic target for this loop. Install
it with the one-line bootstrapper; it writes a `docker-compose.yml` and `.env`
into an `appwrite/` directory:

```bash
docker run -it --rm \
    --volume /var/run/docker.sock:/var/run/docker.sock \
    --volume "$(pwd)"/appwrite:/usr/src/code/appwrite:rw \
    --entrypoint="install" \
    appwrite/appwrite:1.8.0
```

See the [Appwrite self-hosting docs](https://appwrite.io/docs/advanced/self-hosting)
for production setup and configuration. Everything below runs from the generated
`appwrite/` directory.

The executor pulls runtime images from the registry before running them, so a
locally-built image of the same tag is **overwritten by the pull** unless you
disable it. That toggle is the whole trick.

1. **Build the image with its published tag** (not the test tag). The bake
   target `<runtime>-<version>` produces `openruntimes/<runtime>:v5-<version>`:

   ```bash
   docker buildx bake swift-6_2 --load --set '*.tags=openruntimes/swift:v5-6.2'
   docker buildx bake static-1  --load --set '*.tags=openruntimes/static:v5-1'
   ```

2. **Tell the executor to use the local image instead of pulling.** In the
   `appwrite/` directory, add a compose override next to the generated
   `docker-compose.yml` (don't edit the generated file):

   ```yaml
   # docker-compose.override.yml
   services:
     openruntimes-executor:
       environment:
         - OPR_EXECUTOR_IMAGE_PULL=disabled
   ```

   > The default is `OPR_EXECUTOR_IMAGE_PULL=enabled`. Confirm `disabled` is the
   > accepted value in your executor version before relying on it.

3. **Bring it up** with `docker compose up -d`, then create and deploy a
   function on that runtime (via the console at http://localhost or the Appwrite
   CLI) and execute it.

4. **Verify**: the function builds and executes, and
   `docker logs openruntimes-executor` shows the executor using the local image
   with no registry pull.

## Loop 3 — end-to-end through edge (kind/Tilt)

Edge runs on a kind cluster with `imagePullPolicy: IfNotPresent`, and pre-pulls
runtime images listed under `imagePreload.images` in
`deploy/edge-k8s/values.yaml`. To use a locally-built image you load it into the
kind node so the pull is satisfied locally.

1. Build the images with their published tags (same as Loop 2, step 1).

2. **Load them into the kind cluster:**

   ```bash
   kind load docker-image openruntimes/swift:v5-6.2 --name edge
   ```

3. Make sure the tag is in `imagePreload.images` (add it via a values override
   if missing) and that the edge executor also has image-pull disabled so it
   won't re-pull over the loaded image.

4. `tilt up`, deploy a function on that runtime, execute it.

5. **Verify**: `kubectl describe pod <runtime-pod>` shows the image was used
   locally (no pull), and the execution succeeds.

## Which loop to use

| Situation | Loop |
|---|---|
| Adding/changing a runtime, its Dockerfile, or its hooks | **1** (the harness) |
| Changing build/start lifecycle or the executor contract | **1**, then **2** |
| Confirming an image runs under a real executor before release | **2** |
| Prod-parity smoke for the edge deployment model | **3** |

Edge mostly re-confirms what Community Edition already proves, so treat it as a
final prod-parity check rather than your main loop.
