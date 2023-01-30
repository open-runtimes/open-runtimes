#!/bin/sh

set -e

build() {
    cp -a /usr/code/. /usr/local/src/

    cd /usr/local/src/

    # Apply a (cs|fs|vb)proj file to the root project if found
    for filename in *.*proj; do
        if [ ! -f "${filename}" ] || [ "${filename}" = "DotNetRuntime.csproj" ]; then
            continue
        fi

        # This is somewhat brittle - won't handle PackageReferences split across multiple lines. 
        # Only occurs in the case of manually editing the csproj file. A solution is parsing the csproj file as XML.
        PACKAGE_REFS=""
        while read line; do
            case "${line}" in *PackageReference*)
                PACKAGE_REFS="\\t\\t${PACKAGE_REFS}${line}\\n"
            esac
        done < "$filename"

        PACKAGE_REFS="\\t<ItemGroup>\\n${PACKAGE_REFS}\\t</ItemGroup>"
        sed -i "s|</Project>|${PACKAGE_REFS}\\n</Project>|" DotNetRuntime.csproj
        rm "$filename"
        break
    done


    case ${OPEN_RUNTIMES_ENTRYPOINT##*.} in 
        cs) write_cs_wrapper ;;
        fs) write_fs_wrapper ;;
        vb) write_vb_wrapper ;;
    esac

    # Remove the user code file (copy)
    rm "${OPEN_RUNTIMES_ENTRYPOINT}"

    # Build the executable
    cd /usr/local/src
    dotnet publish DotNetRuntime.csproj -c Release

    # Tar the executable
    cd /usr/local/src/bin/Release/net6.0/publish/
    tar -czf /usr/code/code.tar.gz .
}

write_cs_wrapper() {
    # Read user code and collect usings
    USINGS=""
    while read line; do
        case "${line}" in using*)
            USINGS="${USINGS}${line}
            "
        esac
    done < "$OPEN_RUNTIMES_ENTRYPOINT"
    CODE="$(sed /using*/d "$OPEN_RUNTIMES_ENTRYPOINT")"

    # Wrap the user code in a class
    echo "${USINGS}
    namespace DotNetRuntime;
    public class Wrapper {
        ${CODE}
    }
    " > Wrapper.cs
}

write_fs_wrapper() {
    # Read user code and collect opens
    OPENS=""
    while read line; do
        case "${line}" in open*)
            OPENS="${OPENS}${line}
            "
        esac
    done < "$OPEN_RUNTIMES_ENTRYPOINT"
    CODE="$(sed /open*/d "$OPEN_RUNTIMES_ENTRYPOINT" | sed 's/^/        /')"
    # Wrap the user code in a class
    echo "namespace DotNetRuntime
    ${OPENS}
    type Wrapper()=
${CODE}
    " > Wrapper.fs

    cat Wrapper.fs
}

write_vb_wrapper() {
    # Read user code and collect imports
    IMPORTS=""
    while read line; do
        case "${line}" in Imports*)
            IMPORTS="${IMPORTS}${line}
            "
        esac
    done < "$OPEN_RUNTIMES_ENTRYPOINT"
    CODE="$(sed /using*/d "$OPEN_RUNTIMES_ENTRYPOINT")"

    # Wrap the user code in a class
    echo "${IMPORTS}
    Namespace DotNetRuntime
        Public Class Wrapper
            ${CODE}
        End Class
    End Namespace" > Wrapper.vb
}

build