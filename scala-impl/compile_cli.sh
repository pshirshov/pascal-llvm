#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <input.pas> <output>"
    exit 1
fi

INPUT="$1"
OUTPUT="$2"

# Use scala-cli to run the compiler directly
scala-cli run \
  --server=false \
  src/main/scala/pascal/Main.scala \
  src/main/scala/pascal/AST.scala \
  src/main/scala/pascal/Parser.scala \
  src/main/scala/pascal/TypeChecker.scala \
  src/main/scala/pascal/CodeGen.scala \
  --dependency com.lihaoyi::fastparse:3.1.1 \
  --dependency org.bytedeco:llvm-platform:20.1.7-1.5.12 \
  -- "$INPUT" "$OUTPUT"
