# Continuous Integration Setup

## Overview

The project uses GitHub Actions for continuous integration with the Determinate Nix Installer for reproducible builds.

## Workflow Configuration

Location: `.github/workflows/ci.yml`

### Workflow Triggers

- **Push**: Triggered on pushes to `main` or `master` branches
- **Pull Request**: Triggered on PRs to `main` or `master` branches
- **Manual**: Can be triggered manually via `workflow_dispatch`

### Jobs

#### Build and Test Job

Runs on: `ubuntu-latest`
Timeout: 30 minutes

**Steps:**

1. **Checkout Code**
   - Uses: `actions/checkout@v4`
   - Checks out the repository code

2. **Install Nix**
   - Uses: `DeterminateSystems/nix-installer-action@main`
   - Installs Nix with sandbox disabled for compatibility
   - Configuration:
     ```yaml
     logger: pretty
     extra-conf: |
       sandbox = false
     ```

3. **Setup Nix Cache**
   - Uses: `DeterminateSystems/magic-nix-cache-action@main`
   - Provides fast caching of Nix store artifacts
   - Dramatically speeds up subsequent builds

4. **Show Environment Info**
   - Displays Nix version
   - Shows OCaml and Dune versions
   - Helps with debugging build issues

5. **Build Compiler**
   - Runs: `nix develop --command dune build`
   - Builds the compiler in the Nix development environment
   - All dependencies are provided by the Nix flake

6. **Show Build Artifacts**
   - Lists built files in `_build/default/src/`
   - Useful for verification

7. **Run Test Suite**
   - Runs: `nix develop --command ./test.sh`
   - Executes all 10 automated tests
   - Tests include:
     - Basic functionality (variables, I/O)
     - Recursion (Fibonacci, factorial)
     - Control flow (loops, conditionals)
     - Records (local, parameters, return values)
     - Inline declarations (var, val)

8. **Upload Test Artifacts** (on failure)
   - Uses: `actions/upload-artifact@v4`
   - Uploads test output on failure for debugging
   - Retention: 7 days

## Local Development vs CI

### compile.sh Environment Detection

The `compile.sh` script automatically detects if it's running in a Nix environment:

```bash
if [ -z "$IN_NIX_SHELL" ]; then
  # Set up hardcoded paths for local development
else
  # Use environment provided by nix develop
fi
```

This allows the same script to work in both:
- **Local development**: Uses hardcoded Nix store paths
- **CI environment**: Uses paths from `nix develop` shell

### Portable Shebangs

Both `compile.sh` and `test.sh` use portable shebangs:

```bash
#!/usr/bin/env bash
```

This ensures compatibility across different systems and CI runners.

## Benefits

### Reproducible Builds

- **Nix flake**: Pins all dependencies to specific versions
- **Determinate installer**: Ensures consistent Nix installation
- **Same environment**: CI runs in identical environment to local development

### Fast Builds

- **Magic Nix Cache**: Caches build artifacts across runs
- **Incremental builds**: Only rebuilds what changed
- **Parallel compilation**: Dune builds in parallel

### Comprehensive Testing

- **10 test cases**: Cover all language features
- **Automated verification**: Checks output of compiled programs
- **Regression prevention**: Catches breaking changes immediately

## Troubleshooting

### Build Failures

Check the "Show environment info" step to verify:
- Nix version
- OCaml version (should be 5.2.1)
- Dune version (should be 3.20.2)

### Test Failures

Download test artifacts from the failed run:
- Navigate to the failed workflow run
- Scroll to "Artifacts" section
- Download "test-output" artifact
- Contains logs from each failed test

### Sandbox Issues

The workflow disables Nix sandbox mode:
```yaml
extra-conf: |
  sandbox = false
```

This is necessary for LLVM compilation. If you encounter sandbox-related errors, verify this configuration is present.

## Updating the Workflow

### Changing Nix Installer Version

Currently uses `@main` for latest version. To pin to a specific version:

```yaml
- name: Install Nix
  uses: DeterminateSystems/nix-installer-action@v14  # or latest version
```

### Adding More Test Cases

1. Add test case to `test.sh`:
   ```bash
   run_test "test_name" "examples/test.pas" "expected_output"
   ```

2. Push changes - CI will automatically run new test

### Modifying Build Steps

Edit `.github/workflows/ci.yml` to add or modify build steps. Common additions:
- Code formatting checks
- Linting
- Documentation generation
- Binary artifact uploads

## Integration with GitHub

### Status Badges

Add to README.md:
```markdown
![CI](https://github.com/USERNAME/REPO/workflows/CI/badge.svg)
```

Replace `USERNAME` and `REPO` with your GitHub username and repository name.

### Protected Branches

Consider requiring CI to pass before merging:
1. Go to Settings → Branches
2. Add branch protection rule for `main`
3. Enable "Require status checks to pass before merging"
4. Select the "Build and Test" check

## Performance

Typical run times:
- **First run**: ~5-10 minutes (downloading dependencies)
- **Cached runs**: ~2-3 minutes (magic nix cache hits)
- **Build only**: ~1-2 minutes
- **Tests only**: ~30-60 seconds

## Future Improvements

Potential enhancements:
- [ ] Matrix builds for multiple platforms
- [ ] Benchmark tracking
- [ ] Code coverage reporting
- [ ] Release automation
- [ ] Binary artifact publishing
