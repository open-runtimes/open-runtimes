# Local development entrypoints. Requires bun and docker.
#
#   make test ID=node-25       full test cycle for one runtime
#   make test ID=next-js       SSR alias
#   make image ID=node-25      build the test image only
#   make format ID=node        run formatters in write mode
#   make bake                  regenerate docker-bake.json from ci/runtimes.toml
#   make clean                 remove test containers and staged fixtures

ID ?= node-latest

.PHONY: test image format bake clean

test:
	bun ci/test.ts $(ID)

image:
	bun ci/test.ts $(ID) --image-only

format:
	bun ci/test.ts $(ID) --format-write

bake:
	bun ci/bake.ts

clean:
	docker compose -f tests/compose.yaml down --remove-orphans 2>/dev/null || true
	rm -rf tests/.runtime /tmp/logs
