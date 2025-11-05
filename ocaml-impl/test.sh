#!/usr/bin/env bash
# Automated test runner for Pascal compiler

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

TESTS_PASSED=0
TESTS_FAILED=0
TEST_DIR="test_output"

# Create temporary test directory
mkdir -p "$TEST_DIR"

# Check if we're in a nix environment (or commands are available)
if ! command -v dune &> /dev/null || ! command -v llc &> /dev/null; then
    echo -e "${RED}ERROR: Required tools not found${NC}"
    echo ""
    echo "Please ensure you're in the Nix environment:"
    echo "  cd ocaml-impl && nix develop"
    echo "Or use direnv:"
    echo "  cd ocaml-impl && direnv allow"
    exit 1
fi

# Build the compiler first
echo -n "Building compiler... "
if dune build > "$TEST_DIR/build.log" 2>&1; then
    echo -e "${GREEN}OK${NC}"
else
    echo -e "${RED}FAILED${NC}"
    cat "$TEST_DIR/build.log"
    exit 1
fi
echo ""

# Function to run a test
run_test() {
    local test_name="$1"
    local pas_file="$2"
    local expected_output="$3"

    echo -n "Testing $test_name... "

    local binary="$TEST_DIR/${test_name}_bin"

    # Compile
    if ! ./compile.sh "$pas_file" "$binary" > "$TEST_DIR/${test_name}.log" 2>&1; then
        echo -e "${RED}FAILED${NC} (compilation error)"
        cat "$TEST_DIR/${test_name}.log"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi

    # Run and capture output
    local actual_output
    if ! actual_output=$("$binary" 2>&1); then
        echo -e "${RED}FAILED${NC} (runtime error)"
        echo "Output: $actual_output"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi

    # Compare output
    if [ "$actual_output" = "$expected_output" ]; then
        echo -e "${GREEN}PASSED${NC}"
        TESTS_PASSED=$((TESTS_PASSED + 1))
        return 0
    else
        echo -e "${RED}FAILED${NC} (output mismatch)"
        echo "Expected: '$expected_output'"
        echo "Got:      '$actual_output'"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi
}

echo "========================================="
echo "Pascal Compiler Test Suite"
echo "========================================="
echo ""

# Run tests
run_test "simple_var" "../examples/test2.pas" "42"
run_test "fibonacci_recursive" "../examples/fibonacci.pas" "5"
run_test "factorial_recursive" "../examples/factorial_demo.pas" "120"
run_test "loops_for" "../examples/loops_test.pas" "55"
run_test "arithmetic_expr" "../examples/arithmetic_test.pas" "20"
run_test "record_local" "../examples/record_local.pas" "30"
run_test "record_param" "../examples/record_param.pas" "40"
run_test "record_return" "../examples/record_return.pas" "100
200"
run_test "inline_var" "../examples/inline_var.pas" "30
35"
run_test "inline_val" "../examples/inline_val.pas" "30"

echo ""
echo "========================================="
echo "Test Results"
echo "========================================="
echo -e "${GREEN}Passed: $TESTS_PASSED${NC}"
if [ $TESTS_FAILED -gt 0 ]; then
    echo -e "${RED}Failed: $TESTS_FAILED${NC}"
else
    echo "Failed: $TESTS_FAILED"
fi
echo "========================================="

# Cleanup
rm -rf "$TEST_DIR"

# Exit with error code if any tests failed
if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
fi

exit 0
