#!/usr/bin/env bash
# Automated test runner for Pascal compiler (Rust implementation)

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

echo "========================================="
echo "Pascal Compiler Test Suite (Rust)"
echo "========================================="
echo ""

# Check if cargo is available
if ! command -v cargo &> /dev/null; then
    echo -e "${RED}ERROR: cargo not found${NC}"
    echo ""
    echo "Please ensure you're in the Nix environment:"
    echo "  cd rust-impl && nix develop"
    echo "Or use direnv:"
    echo "  cd rust-impl && direnv allow && direnv exec . ./test.sh"
    exit 1
fi

# Build the compiler
echo -n "Building compiler... "
if cargo build --release > "$TEST_DIR/build.log" 2>&1; then
    echo -e "${GREEN}OK${NC}"
else
    echo -e "${RED}FAILED${NC}"
    cat "$TEST_DIR/build.log"
    exit 1
fi

# Run unit tests
echo -n "Running unit tests... "
if cargo test > "$TEST_DIR/unit_tests.log" 2>&1; then
    echo -e "${GREEN}OK${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}FAILED${NC}"
    cat "$TEST_DIR/unit_tests.log"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi

# Function to run an integration test
run_test() {
    local test_name="$1"
    local pas_file="$2"
    local expected_output="$3"

    echo -n "Testing $test_name... "

    local binary="$TEST_DIR/${test_name}_bin"

    # Compile
    if ! ./target/release/pascalc "$pas_file" -o "${test_name}.ll" > "$TEST_DIR/${test_name}.log" 2>&1; then
        echo -e "${RED}FAILED${NC} (compilation error)"
        cat "$TEST_DIR/${test_name}.log"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi

    # Compile LLVM IR to binary
    if ! llc "${test_name}.ll" -o "${test_name}.s" 2>> "$TEST_DIR/${test_name}.log"; then
        echo -e "${RED}FAILED${NC} (llc error)"
        cat "$TEST_DIR/${test_name}.log"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi

    if ! gcc -no-pie "${test_name}.s" -o "$binary" 2>> "$TEST_DIR/${test_name}.log"; then
        echo -e "${RED}FAILED${NC} (gcc error)"
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

# Check if code generator is implemented
if cargo build --release 2>&1 | grep -q "not yet implemented"; then
    echo ""
    echo -e "${YELLOW}Note: Code generation not yet implemented${NC}"
    echo -e "${YELLOW}Skipping integration tests${NC}"
else
    echo ""
    echo "Running integration tests..."
    echo ""

    # Run integration tests (same as OCaml implementation)
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
fi

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
rm -f *.ll *.s

# Exit with error code if any tests failed
if [ $TESTS_FAILED -gt 0 ]; then
    exit 1
fi

exit 0
