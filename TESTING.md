# Testing Guide

## Local Testing Limitations

**⚠️ Important**: If you encounter nix sandbox errors (`error: this system does not support the kernel namespaces`), local testing may not work. This is a system limitation, not a bug in the code.

### Why This Happens

Nix requires kernel namespace support for sandboxing package builds. Some systems don't support this:
- Older Linux kernels
- WSL (Windows Subsystem for Linux)
- Docker containers without privileged mode
- Systems with restricted capabilities

After reorganizing the repository into subdirectories, nix needs to rebuild the development environments, which triggers the sandbox requirement.

### Workarounds

####1. **CI Testing (Recommended)**

GitHub Actions doesn't have these limitations. Push your changes and let CI run the tests:

```bash
git add -A
git commit -m "Your changes"
git push
```

Then check the Actions tab on GitHub to see test results.

#### 2. **Pre-built Environment**

If you have a working build from before the reorganization:

```bash
# OCaml
cd ocaml-impl
./_build/default/src/main.exe ../examples/fibonacci.pas -o test.ll
llc test.ll -o test.s
gcc test.s -o test
./test

# Rust (once implemented)
cd rust-impl
./target/release/pascalc ../examples/fibonacci.pas -o test.ll
```

#### 3. **Skip Nix (Advanced)**

Install dependencies manually without nix:

**OCaml**:
```bash
# Install via opam
opam install dune menhir llvm
cd ocaml-impl
dune build
./test.sh
```

**Rust**:
```bash
# Install Rust and LLVM
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
# Install LLVM 18 (distribution-specific)

cd rust-impl
export LLVM_SYS_180_PREFIX=/usr/lib/llvm-18
cargo build --release
./test.sh
```

## CI Testing

The repository is configured with GitHub Actions that test both implementations:

### Workflow Structure

`.github/workflows/ci.yml` defines two parallel jobs:

1. **test-ocaml**: Tests OCaml implementation
   - Runs `ocaml-impl/test.sh`
   - Tests 10 example programs
   - Uploads artifacts on failure

2. **test-rust**: Tests Rust implementation
   - Runs `rust-impl/test.sh`
   - Runs unit tests
   - Runs integration tests (when code generation is complete)
   - Uploads artifacts on failure

### Test Scripts

Each implementation has its own `test.sh` script:

**`ocaml-impl/test.sh`**:
- Builds the compiler with dune
- Compiles and runs 10 test programs
- Verifies output matches expected results
- Returns exit code 0 on success, 1 on failure

**`rust-impl/test.sh`**:
- Builds with `cargo build --release`
- Runs `cargo test` for unit tests
- Compiles and runs integration tests (when implemented)
- Gracefully skips integration tests if code generation isn't ready

**`test-all.sh`** (root):
- Runs both implementation test suites
- Reports results for each
- Useful for local testing when nix works

### CI Features

- **Parallel Testing**: Both implementations test simultaneously
- **Determinate Nix**: Uses Determinate Systems Nix installer for reliability
- **Magic Nix Cache**: Caches nix store for faster builds (2-3 min vs 5-10 min)
- **Sandbox Disabled**: CI explicitly sets `sandbox = false` to avoid restrictions
- **Artifact Upload**: Failed test logs are preserved for 7 days

### Viewing CI Results

1. Go to your repository on GitHub
2. Click the "Actions" tab
3. Click on the latest workflow run
4. See results for both "OCaml Implementation" and "Rust Implementation"
5. If tests fail, download artifacts for debugging

## Test Coverage

### OCaml Implementation

All 10 tests passing:

| Test | Program | Verifies |
|------|---------|----------|
| simple_var | test2.pas | Basic I/O and variables |
| fibonacci_recursive | fibonacci.pas | Recursion |
| factorial_recursive | factorial_demo.pas | Recursion with parameters |
| loops_for | loops_test.pas | For loops and accumulation |
| arithmetic_expr | arithmetic_test.pas | Complex expressions |
| record_local | record_local.pas | Local record usage |
| record_param | record_param.pas | Records as parameters |
| record_return | record_return.pas | Records as return values |
| inline_var | inline_var.pas | Mutable inline declarations |
| inline_val | inline_val.pas | Immutable inline declarations |

### Rust Implementation

Status: 🚧 In Development

- ✅ Lexer unit tests
- ✅ Parser unit tests
- ⏳ Type checker tests (pending implementation)
- ⏳ Code generator tests (pending implementation)
- ⏳ Integration tests (pending code generation)

## Adding New Tests

1. Create a test program in `examples/`

2. Add to `ocaml-impl/test.sh`:
```bash
run_test "test_name" "../examples/your_test.pas" "expected
output"
```

3. Add to `rust-impl/test.sh`:
```bash
run_test "test_name" "../examples/your_test.pas" "expected
output"
```

4. Commit and push - CI will run the new test

## Debugging Failed Tests

### In CI

1. Click on the failed job
2. Expand the "Run ... test suite" step
3. See which test failed and why
4. Download artifacts if available

### Locally (if nix works)

```bash
cd ocaml-impl  # or rust-impl
nix develop --command ./test.sh
```

Look in `test_output/` for detailed logs.

## Summary

**Local Testing**: May not work due to nix sandbox limitations
**CI Testing**: Always works, use this for verification
**Manual Testing**: Possible with pre-built binaries
**Test Scripts**: Located in each implementation directory
**Test Programs**: Shared in `examples/` directory
