SERVER_CODE="$(cat "src/main.cc")"
SERVER_CODE=$(echo "$SERVER_CODE" | sed "s/{entrypointFile}/$OPEN_RUNTIMES_ENTRYPOINT/g")
echo "$SERVER_CODE" > src/main.cc