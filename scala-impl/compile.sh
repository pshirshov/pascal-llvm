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

# Run the compiler
java -jar target/scala-3.5.2/pascal-compiler.jar "$INPUT" "$OUTPUT"
