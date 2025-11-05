#!/usr/bin/env bash
# Test runner for all compiler implementations

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

OCAML_FAILED=0
RUST_FAILED=0

echo "========================================="
echo "Testing All Implementations"
echo "========================================="
echo ""

# Check if nix is available
if ! command -v nix &> /dev/null; then
    echo -e "${RED}ERROR: nix not found in PATH${NC}"
    echo "Please install nix or ensure it's in your PATH"
    exit 1
fi

# Test OCaml implementation
echo -e "${BLUE}==> Testing OCaml Implementation${NC}"
echo ""
cd ocaml-impl
if nix develop --command ./test.sh 2>&1; then
    echo ""
    echo -e "${GREEN}✓ OCaml implementation: All tests passed${NC}"
else
    OCAML_FAILED=1
    echo ""
    echo -e "${RED}✗ OCaml implementation: Some tests failed${NC}"
fi
cd ..

echo ""
echo "========================================="
echo ""

# Test Rust implementation
echo -e "${BLUE}==> Testing Rust Implementation${NC}"
echo ""
cd rust-impl
if nix develop --command ./test.sh 2>&1; then
    echo ""
    echo -e "${GREEN}✓ Rust implementation: All tests passed${NC}"
else
    RUST_FAILED=1
    echo ""
    echo -e "${RED}✗ Rust implementation: Some tests failed${NC}"
fi
cd ..

echo ""
echo "========================================="
echo "Summary"
echo "========================================="

if [ $OCAML_FAILED -eq 0 ] && [ $RUST_FAILED -eq 0 ]; then
    echo -e "${GREEN}All implementations passed!${NC}"
    exit 0
else
    if [ $OCAML_FAILED -ne 0 ]; then
        echo -e "${RED}✗ OCaml implementation failed${NC}"
    fi
    if [ $RUST_FAILED -ne 0 ]; then
        echo -e "${RED}✗ Rust implementation failed${NC}"
    fi
    exit 1
fi
