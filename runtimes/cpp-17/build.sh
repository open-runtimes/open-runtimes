#!/usr/bin/env sh

# Fail build if any command fails
set -e

mkdir -p /usr/builds
cp -R /usr/code/* /usr/builds

if [[ ! -f "usr/builds/CMakeLists.txt" ]]; then
    mv /usr/local/src/CMakeLists.txt.fallback /usr/builds/CMakeLists.txt
fi

cd /usr/local/src

# Append User Function Dependencies"
cat "/usr/builds/CMakeLists.txt" >> "CMakeLists.txt"

# Prepare separate directory to prevent changing user's files
cp -R --no-clobber /usr/code/* /usr/local/src

# Read user code and collect includes and usings
INCLUDES=""
while read -r line; do
    case "${line}" in "#"*|"using"*)
        INCLUDES="${INCLUDES}${line}
        "
    esac
done < "${INTERNAL_RUNTIME_ENTRYPOINT}"
CODE="$(sed "/#include*/d" "${INTERNAL_RUNTIME_ENTRYPOINT}")"

# Wrap the user code in a class
echo "
#ifndef CPP_RUNTIME_WRAPPER_H
#define CPP_RUNTIME_WRAPPER_H
#include \"RuntimeResponse.h\"
#include \"RuntimeRequest.h\"
${INCLUDES}
namespace runtime {
    class Wrapper {
    public:
        ${CODE}
    };
}
#endif //CPP_RUNTIME_WRAPPER_H
" > src/Wrapper.h

# Remove the user code file
rm "${INTERNAL_RUNTIME_ENTRYPOINT}"
rm "CMakeCache.txt" >/dev/null || true

# Build the executable
cd /usr/local/src/build

INSTALL_COMMAND=${1:-'cmake -DCMAKE_BUILD_TYPE=Release ..'}
BUILD_COMMAND=${2:-'make -j"$(nproc)"'}

eval "$INSTALL_COMMAND"
eval "$BUILD_COMMAND"

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .