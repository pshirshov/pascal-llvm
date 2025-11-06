#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <input.pas> <output>"
    exit 1
fi

INPUT="$1"
OUTPUT="$2"

# Build the compiler if needed
if [ ! -f "target/scala-3.5.2/pascal-compiler.jar" ]; then
    echo "Building compiler..."
    sbt assembly
fi

# Set JavaCPP library path for LLVM native bindings
# This allows JavaCPP to find the bundled LLVM libraries in the JAR
export JAVACPP_CACHE_DIR="${TMPDIR:-/tmp}/.javacpp-cache"
mkdir -p "$JAVACPP_CACHE_DIR"

# Run the compiler
java -Djava.io.tmpdir="$JAVACPP_CACHE_DIR" \
     -jar target/scala-3.5.2/pascal-compiler.jar "$INPUT" "$OUTPUT"
