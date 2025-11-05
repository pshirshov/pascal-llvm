# Development Guide

## Repository Structure

```
.
├── .github/              # CI/CD workflows
├── examples/             # Shared test programs
├── ocaml-impl/          # OCaml implementation
│   ├── src/             # Source code
│   ├── compile.sh       # Compilation wrapper
│   ├── test.sh          # Test runner
│   └── flake.nix        # Nix environment
├── rust-impl/           # Rust implementation (in progress)
│   ├── src/             # Source code
│   ├── test.sh          # Test runner
│   └── flake.nix        # Nix environment
└── test-all.sh          # Run all implementations
```

## Testing

### Individual Implementations

```bash
# Test OCaml implementation
cd ocaml-impl
nix develop --command ./test.sh

# Test Rust implementation
cd rust-impl
nix develop --command ./test.sh
```

### All Implementations

```bash
./test-all.sh
```

This script:
- Tests both OCaml and Rust implementations
- Runs in parallel in CI
- Reports success/failure for each

## Known Issues

### Nix Sandbox on Systems Without Namespace Support

Some systems (e.g., certain WSL configurations, older kernels, or systems with restricted capabilities) don't support kernel namespaces required for Nix sandboxing.

**Symptoms:**
```
error: this system does not support the kernel namespaces that are required for sandboxing
```

**Workarounds:**

1. **Set global sandbox config** (already done if you see this):
   ```bash
   echo "sandbox = false" >> ~/.config/nix/nix.conf
   ```

2. **Use `--option` flag** (may not work for all operations):
   ```bash
   nix develop --option sandbox false
   ```

3. **Use binary cache only** (avoid building):
   The OCaml implementation works because packages are in the binary cache. The Rust implementation may require building packages not in cache.

4. **CI Environment**:
   GitHub Actions doesn't have this limitation - tests will work in CI even if they fail locally.

**Resolution:**
The CI workflow in `.github/workflows/ci.yml` already includes `sandbox = false` configuration and will work correctly on GitHub Actions runners.

## Adding New Tests

1. Create a `.pas` file in `examples/`
2. Add a test case in `ocaml-impl/test.sh` and `rust-impl/test.sh`:
   ```bash
   run_test "test_name" "../examples/your_test.pas" "expected_output"
   ```

## Continuous Integration

The CI workflow (`.github/workflows/ci.yml`) runs both implementations in parallel:

- `test-ocaml`: Tests OCaml implementation
- `test-rust`: Tests Rust implementation

Each job:
1. Checks out code
2. Installs Nix with Determinate Systems installer
3. Enables Magic Nix Cache for faster builds
4. Runs the implementation's test script
5. Uploads artifacts on failure

The workflow uses the test scripts (`./test.sh`) in each implementation directory, making it easy to test locally with the same commands that run in CI.

## Development Workflow

### OCaml Implementation

```bash
cd ocaml-impl
direnv allow  # or: nix develop
dune build
./compile.sh ../examples/fibonacci.pas fib
./fib
```

### Rust Implementation

```bash
cd rust-impl
direnv allow  # or: nix develop
cargo build --release
cargo run -- ../examples/fibonacci.pas -o fibonacci.ll
# (code generation not yet implemented)
```

## Creating a New Implementation

To add a third implementation (e.g., C++, Haskell):

1. Create `<lang>-impl/` directory
2. Add `flake.nix` for dependencies
3. Create `test.sh` following the pattern in other implementations
4. Add job to `.github/workflows/ci.yml`:
   ```yaml
   test-<lang>:
     name: <Lang> Implementation
     runs-on: ubuntu-latest
     steps:
       - uses: actions/checkout@v4
       - uses: DeterminateSystems/nix-installer-action@main
       - uses: DeterminateSystems/magic-nix-cache-action@main
       - working-directory: ./<lang>-impl
         run: nix develop --command ./test.sh
   ```
5. Update `test-all.sh` to include the new implementation

## Troubleshooting

### "cargo: command not found" in Rust tests

Ensure you're in the Nix environment:
```bash
cd rust-impl
nix develop
```

### "ocaml: command not found" in OCaml tests

Ensure you're in the Nix environment:
```bash
cd ocaml-impl
nix develop
```

### Direnv not loading automatically

```bash
direnv allow
```

### Tests pass locally but fail in CI (or vice versa)

- Check that `.github/workflows/ci.yml` uses the same test commands
- Verify `test.sh` scripts are executable: `chmod +x */test.sh test-all.sh`
- Ensure paths in test scripts are relative (use `../examples/` not `examples/`)
