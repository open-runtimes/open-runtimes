#!/bin/sh

# Fail build if any command fails
set -e

cp -a /usr/code/. /usr/local/src
cd /usr/local/src

# Extract uses from the Server source as well as the function
server_uses="$(grep -zoP '(?s)(?<=uses).*?(?=;)' Server.pas | tr -d '\0')"
function_uses="$(grep -zoP '(?s)(?<=uses).*?(?=;)' $INTERNAL_RUNTIME_ENTRYPOINT | tr -d '\0' || true)"

total_uses="$server_uses"
if [ -n "$function_uses" ]; then
  total_uses="$server_uses, $function_uses"
fi

# Dedupe uses (it's a compiler error to have duplicate uses)
total_uses="uses $(echo $total_uses | tr ',' '\n' | xargs -n1 | sort -u | xargs | tr ' ' ',');"

# Replace the uses in the server with the joint uses from above
sed -i "s/uses[^;]*;/$total_uses/" Server.pas

# Collect all lines that don't belong to the uses statement and replace the placeholder
is_relevant=1
function=''
while read -r line; do
  if echo "$line" | grep -q 'uses.*'; then
    is_relevant=0
    continue
  fi

  if echo "$line" | grep -q '.*;'; then
    if [ $is_relevant -eq 0 ]; then
      is_relevant=1
      continue
    fi
  fi

  if [ $is_relevant -eq 1 ]; then
    function="${function}${line}\n"
  fi
done < "$INTERNAL_RUNTIME_ENTRYPOINT"

sed -i "s@{ Function Placeholder }@${function}@" Server.pas

# Compile
fpc Server

# Compress
tar -zcf /usr/code/code.tar.gz .
