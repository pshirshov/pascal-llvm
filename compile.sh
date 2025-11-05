#!/usr/bin/env bash
# Pascal compiler wrapper script

set -e

# Only set up paths if not already in a nix environment
if [ -z "$IN_NIX_SHELL" ]; then
  # Set up environment paths for local development
  export PATH="/nix/store/416ykpc2bksb90sd1ia8cybxb3p83mrd-binutils-2.44/bin:/nix/store/q78azngpiiy1xgdcs1gh5v9qr1k5ds9a-ocaml-5.2.1/bin:/nix/store/igad1zyc8lswmj6h9pm1irp46dy8d3rr-dune-3.20.2/bin:/nix/store/xd28n24yjmd9wwfgv3kn6kpmqpk3aklx-ocaml5.2.1-menhir-20250903/bin:/nix/store/ffrg0560kj0066s4k9pznjand907nlnz-gcc-14.3.0/bin:/nix/store/sq5fmg07kf6aqrxs8d8kikmz8sh4ynfz-llvm-18.1.8/bin:$PATH"

  export OCAMLPATH="/nix/store/4563vxvg3ffwxqhc5rd2qyb63mjwi49c-ocaml-llvm-21.1.2/lib/ocaml/5.2.1/site-lib"

  export LIBRARY_PATH="/nix/store/daamdpmaz2vjvna55ccrc30qw3qb8h6d-glibc-2.40-66/lib:/nix/store/krcdpgjd3h5qdpiz1iar1krffwx6sn2j-llvm-21.1.2-lib/lib:/nix/store/z55x0q74zldi64iwamqf8wgrm2iza5rk-zlib-1.3.1/lib:/nix/store/ffrg0560kj0066s4k9pznjand907nlnz-gcc-14.3.0/lib:/nix/store/z7a34j3xnp66rpddayyxrxwsahxccbip-gcc-14.3.0-lib/lib"

  export LD_LIBRARY_PATH="/nix/store/krcdpgjd3h5qdpiz1iar1krffwx6sn2j-llvm-21.1.2-lib/lib:/nix/store/z55x0q74zldi64iwamqf8wgrm2iza5rk-zlib-1.3.1/lib:/nix/store/daamdpmaz2vjvna55ccrc30qw3qb8h6d-glibc-2.40-66/lib:$LD_LIBRARY_PATH"

  export C_INCLUDE_PATH="/nix/store/daamdpmaz2vjvna55ccrc30qw3qb8h6d-glibc-2.40-66/include"
fi

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
gcc "${BASENAME}.s" -o "$OUTPUT"

echo "Successfully compiled to: $OUTPUT"
echo "Run with: ./$OUTPUT"
