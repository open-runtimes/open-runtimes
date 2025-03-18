# Merge dependencies
# Apply a (cs|fs|vb)proj file to the root project if found
cp -a /usr/local/build/. /usr/local/server/
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

echo "Compiling ..."

# Prepare folder for compiled build
mkdir /usr/local/build/compiled

# Compile the Code
cd /usr/local/server
dotnet publish DotNetRuntime.csproj -c Release

# Copy output files
cp -R /usr/local/server/bin/Release/net7.0/publish/. /usr/local/build/compiled
