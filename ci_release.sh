set -e

sh ci-helpers.sh
sh ci-cleanup.sh
sh ci-runtime-prepare.sh