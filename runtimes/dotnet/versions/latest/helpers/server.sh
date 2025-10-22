#!/bin/bash
# Fail build if any command fails
set -e
shopt -s dotglob

dotnet src/function/DotNetRuntime.dll
