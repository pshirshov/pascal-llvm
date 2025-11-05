#!/usr/bin/env bash
# Quick build test without nix sandbox

set -e

echo "========================================="
echo "Quick Rust Build Test (No Sandbox)"
echo "========================================="
echo

# Check for cargo
if ! command -v cargo &> /dev/null; then
    echo "ERROR: cargo not found in PATH"
    echo "Try: export PATH=\$PATH:~/.cargo/bin"
    exit 1
fi

# Check for LLVM
if ! command -v llvm-config &> /dev/null; then
    echo "WARNING: llvm-config not found"
    echo "You may need to set LLVM_SYS_181_PREFIX"
fi

echo "Building..."
cargo build --release

if [ $? -eq 0 ]; then
    echo
    echo "✓ Build successful!"
    echo "Binary: target/release/pascalc"

    # Test if it runs
    if [ -f target/release/pascalc ]; then
        echo
        echo "Testing binary..."
        ./target/release/pascalc --help || true
    fi
else
    echo
    echo "✗ Build failed"
    exit 1
fi
