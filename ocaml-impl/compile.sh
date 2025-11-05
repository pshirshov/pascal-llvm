#!/usr/bin/env bash
# Pascal compiler wrapper script

set -e

# Check for required tools
for tool in dune llc gcc; do
    if ! command -v "$tool" &> /dev/null; then
        echo "ERROR: $tool not found in PATH"
        echo "Please run this script from within the nix environment:"
        echo "  nix develop --command ./compile.sh ..."
        exit 1
    fi
done

if [ $# -eq 0 ]; then
    echo "Usage: $0 <input.pas> [output]"
    exit 1
fi

INPUT="$1"
BASENAME=$(basename "$INPUT" .pas)
OUTPUT="${2:-$BASENAME}"

echo "Compiling $INPUT..."

# Compile Pascal to LLVM IR
_build/default/src/main.exe "$INPUT" -o "${BASENAME}.ll"

# Compile LLVM IR to assembly
llc "${BASENAME}.ll" -o "${BASENAME}.s"

# Compile assembly to binary
gcc -no-pie "${BASENAME}.s" -o "$OUTPUT"

echo "Successfully compiled to: $OUTPUT"
echo "Run with: ./$OUTPUT"
