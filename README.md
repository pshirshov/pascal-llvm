# Pascal-like Language Compiler

Two implementations of a simple Pascal-like language compiler built on LLVM.

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

- **Status**: 🚧 In development
- **LLVM Bindings**: Inkwell (Rust LLVM wrapper)
- **Build System**: Cargo
- **Parser**: TBD

[View Rust Implementation →](./rust-impl/)

## Language Features

Both implementations support the same Pascal-like language:

- **Data Types**: Integer, Real, Boolean, Char, String
- **Composite Types**: Arrays, Records (structs), Pointers
- **Control Flow**: if/then/else, while, for loops
- **Functions**: Functions and procedures with recursion
- **Inline Declarations**: Scala-style `var` (mutable) and `val` (immutable)
- **I/O**: Console input/output (writeln, write, readln)
- **Pointers**: Address-of (@), dereference (^), dynamic allocation (new)

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

## Example Program

```pascal
program Fibonacci;

function fib(n: integer): integer;
begin
  if n <= 1 then
    return n
  else
    return fib(n-1) + fib(n-2)
end;

function main(): integer;
begin
  val result: integer = fib(5);
  writeln(result);
  return 0
end;

.
```

## Inline Variable Declarations

Both implementations support Scala-style inline declarations:

```pascal
function demo(): integer;
begin
  var counter: integer := 0;    (* mutable *)
  val max: integer = 100;       (* immutable *)

  counter := counter + 1;       (* OK *)
  (* max := 200; *)            (* Error: cannot assign to val *)

  return counter
end;
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

## CI/CD

Both implementations use GitHub Actions with Nix for reproducible builds:

- **OCaml**: Full CI with Determinate Nix Installer
- **Rust**: (To be implemented)

## Implementation Comparison

| Feature | OCaml | Rust |
|---------|-------|------|
| LLVM Bindings | ocaml-llvm | Inkwell |
| Type Safety | Strong | Strong |
| Memory Safety | GC | Ownership |
| Build Speed | Fast (Dune) | Fast (Cargo) |
| Ecosystem | Nix/Opam | Cargo |
| Error Messages | Good | Excellent |
| Status | ✅ Complete | 🚧 In Progress |

## Documentation

- [OCaml Implementation README](./ocaml-impl/README.md)
- [OCaml CI Setup](./ocaml-impl/docs/ci-setup.md)
- [Inline Declarations Guide](./ocaml-impl/docs/inline-declarations.md)
- [Rust Implementation README](./rust-impl/README.md) (coming soon)

## License

MIT (or your preferred license)

## Contributing

Contributions welcome! Please see individual implementation directories for specific contribution guidelines.
