# Pascal-like Language Compiler

[![CI](https://github.com/pshirshov/pascal-llvm/actions/workflows/ci.yml/badge.svg)](https://github.com/pshirshov/pascal-llvm/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Three implementations of a modern Pascal-like language compiler built on LLVM. Features a clean, Scala-inspired syntax with `def` functions, brace blocks, and immutable `val` declarations.

## Implementations

### [OCaml Implementation](./ocaml-impl/)

The original implementation using OCaml 5.2 with LLVM OCaml bindings.

- **Status**: ✅ Complete and tested
- **LLVM Bindings**: ocaml-llvm 21.1
- **Build System**: Dune 3.20
- **Parser Generator**: Menhir
- **Tests**: 10 automated tests, all passing

[View OCaml Implementation →](./ocaml-impl/)

### [Rust Implementation](./rust-impl/)

Alternative implementation using Rust with Inkwell (safe LLVM bindings).

- **Status**: ✅ Complete and tested
- **LLVM Bindings**: Inkwell 0.6 (LLVM 18)
- **Build System**: Cargo
- **Parser**: Hand-written recursive descent
- **Tests**: 10 automated tests, all passing

[View Rust Implementation →](./rust-impl/)

### [Scala Implementation](./scala-impl/)

JVM-based implementation using Scala 3 with fastparse and JavaCPP LLVM bindings.

- **Status**: ✅ Complete and tested
- **LLVM Bindings**: JavaCPP llvm-platform 20.1.7
- **Build System**: SBT 1.10.5
- **Parser**: fastparse 3.1.1 (parser combinators)
- **Tests**: 10 automated tests, all passing

[View Scala Implementation →](./scala-impl/)

## Language Features

All three implementations support the same modern Pascal-like language with Scala-style syntax:

- **Data Types**: Integer, Real, Boolean, Char, String
- **Composite Types**: Arrays, Records (structs), Pointers
- **Control Flow**: `if (cond) { }`, `while (cond) { }`, `for (var = start to end) { }`
- **Functions**: `def name(params): Type = { }` with recursion support
- **Variable Declarations**: `var` (mutable) and `val` (immutable)
- **I/O**: Console input/output (writeln, write, readln)
- **Pointers**: Address-of (@), dereference (^), dynamic allocation (new)
- **Comments**: `//` line comments and `/* */` block comments

## Quick Start

### OCaml Implementation

```bash
cd ocaml-impl
direnv allow  # If using nix
dune build
./compile.sh ../examples/fibonacci.pas fibonacci
./fibonacci  # Output: 5
```

### Rust Implementation

```bash
cd rust-impl
cargo build --release
cargo run -- ../examples/fibonacci.pas -o fibonacci
./fibonacci  # Output: 5
```

### Scala Implementation

```bash
cd scala-impl
nix develop  # Or use direnv
sbt assembly
./compile.sh ../examples/fibonacci.pas fibonacci
./fibonacci  # Output: 5
```

## Example Program

```pascal
program Fibonacci;

def fib(n: Integer): Integer = {
  if (n <= 1) {
    return n
  } else {
    return fib(n-1) + fib(n-2)
  }
}

def main(): Integer = {
  val result: Integer = fib(5)
  writeln(result)
  return 0
}

.
```

## Variable Declarations

All implementations support inline variable declarations:

```pascal
def demo(): Integer = {
  var counter: Integer = 0     // mutable
  val max: Integer = 100       // immutable

  counter = counter + 1        // OK
  // max = 200                 // Error: cannot assign to val

  return counter
}
```

## Testing

### OCaml Tests
```bash
cd ocaml-impl
./test.sh
```

### Rust Tests
```bash
cd rust-impl
cargo test
```

### Scala Tests
```bash
cd scala-impl
./test.sh
```

### All Implementations
```bash
./test-all.sh  # Runs tests for all three implementations
```

## CI/CD

All implementations use GitHub Actions with Nix for reproducible builds:

- **OCaml**: Full CI with Determinate Nix Installer (LLVM 21)
- **Rust**: Full CI with Determinate Nix Installer (LLVM 18)
- **Scala**: Full CI with Determinate Nix Installer (LLVM 20)

## Implementation Comparison

| Feature | OCaml | Rust | Scala |
|---------|-------|------|-------|
| LLVM Bindings | ocaml-llvm 21 | Inkwell 0.6 (LLVM 18) | JavaCPP llvm-platform 20 |
| Parser | Menhir (LALR) | Hand-written | fastparse (combinators) |
| Type Safety | Strong | Strong | Strong |
| Memory Safety | GC | Ownership | GC (JVM) |
| Build Speed | Fast (Dune) | Fast (Cargo) | Medium (SBT) |
| Ecosystem | Nix/Opam | Cargo | JVM/Maven |
| Error Messages | Good | Excellent | Good |
| Status | ✅ Complete | ✅ Complete | ✅ Complete |

## Documentation

- [OCaml Implementation README](./ocaml-impl/README.md)
- [OCaml CI Setup](./ocaml-impl/docs/ci-setup.md)
- [Inline Declarations Guide](./ocaml-impl/docs/inline-declarations.md)
- [Rust Implementation README](./rust-impl/README.md)
- [Scala Implementation README](./scala-impl/README.md)

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contributing

Contributions welcome! Please see individual implementation directories for specific contribution guidelines.
