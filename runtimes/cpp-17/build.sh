#!/usr/bin/env sh

# Fail build if any command fails
set -e

if [ -f "/usr/code/CMakeLists.txt" ]; then
    # Append User Function Dependencies"
    cat "/usr/code/CMakeLists.txt" >> "CMakeLists.txt"
fi

# Prepare separate directory to prevent changing user's files
cp -R --no-clobber /usr/code/* /usr/local/src

# Read user code and collect includes and usings
INCLUDES=""
while read -r line; do
    case "${line}" in "#"*|"using"*)
        INCLUDES="${INCLUDES}${line}
        "
    esac
done < "${OPEN_RUNTIMES_ENTRYPOINT}"
CODE="$(sed "/#include*/d" "${OPEN_RUNTIMES_ENTRYPOINT}")"

# Wrap the user code in a class
echo "
#ifndef CPP_RUNTIME_WRAPPER_H
#define CPP_RUNTIME_WRAPPER_H
#include \"RuntimeResponse.h\"
#include \"RuntimeRequest.h\"
#include \"RuntimeOutput.h\"
#include \"RuntimeContext.h\"
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
rm "${OPEN_RUNTIMES_ENTRYPOINT}"
rm "CMakeCache.txt" >/dev/null || true

# Build the executable
cd /usr/local/src/build
cmake -DCMAKE_BUILD_TYPE=Release ..
make -j"$(nproc)"

# Finish build by preparing tar to use for starting the runtime
tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz .