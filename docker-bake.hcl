# Static bake configuration. The targets themselves live in docker-bake.json,
# generated from ci/runtimes.toml by `bun ci/bake.ts` — bake merges both files
# automatically.
#
# Usage:
#   docker buildx bake node-25 --load --set '*.platform=linux/amd64'
#   docker buildx bake node                # whole family
#   docker buildx bake                     # everything
#   docker buildx bake --print node-25     # inspect resolved config

variable "REGISTRY" {
  default = "openruntimes"
}

# Set to "-rc" for release-candidate publishes
variable "TAG_SUFFIX" {
  default = ""
}

# Cloud builds set this to "-<short-sha>" (e.g. "-a1b2c3d") to additionally
# publish an immutable, commit-pinned tag alongside the mutable one. Empty by
# default, so the SHA tag collapses onto the mutable tag (deduped by bake) and
# OSS publishes are unaffected.
variable "SHA_SUFFIX" {
  default = ""
}

target "_base" {
  context = "."
}
