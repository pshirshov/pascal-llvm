# Pascal-like Language Compiler (Rust Implementation)

Alternative implementation using Rust with Inkwell (safe LLVM bindings).

## Status

🚧 **In Development**

- ✅ Lexer - Complete with all keywords and tokens
- ✅ Parser - Complete recursive descent parser
- ⏳ Type checker - In progress
- ⏳ Code generator - In progress
- ⏳ Test suite - Pending full implementation

## Features

Same language features as the OCaml implementation:

- **Data Types**: Integer, Real, Boolean, Char, String
- **Composite Types**: Arrays, Records (structs), Pointers
- **Control Flow**: if/then/else, while, for loops
- **Functions**: Functions and procedures with recursion
- **Inline Declarations**: Scala-style `var` (mutable) and `val` (immutable)
- **I/O**: Console input/output (writeln, write, readln)
- **Pointers**: Address-of (@), dereference (^), dynamic allocation (new)

## Prerequisites

You need:
- Rust >= 1.70
- Cargo
- LLVM 18 with development headers
- Nix (recommended for reproducible builds)

## Building

### Option 1: Using Nix (Recommended)

```bash
direnv allow
cargo build --release
```

### Option 2: Manual Installation

Install Rust and LLVM 18, then:

```bash
export LLVM_SYS_180_PREFIX=/path/to/llvm-18
cargo build --release
```

## Usage

```bash
# Compile a Pascal program
cargo run -- ../examples/fibonacci.pas -o fibonacci.ll

# View tokens (debugging)
cargo run -- ../examples/fibonacci.pas --print-tokens

# View AST (debugging)
cargo run -- ../examples/fibonacci.pas --print-ast

# Compile to executable (when code generation is complete)
llc fibonacci.ll -o fibonacci.s
gcc fibonacci.s -o fibonacci
./fibonacci
```

## Testing

```bash
# Run unit tests
cargo test

# Run full test suite (when complete)
./test.sh
```

## Implementation Details

The compiler consists of:

1. **Lexer** (`src/lexer.rs`) - Tokenizes input
2. **Parser** (`src/parser.rs`) - Builds AST
3. **Type Checker** (`src/types.rs`) - Validates types and semantics (TODO)
4. **Code Generator** (`src/codegen.rs`) - Generates LLVM IR via Inkwell (TODO)
5. **Main Driver** (`src/main.rs`) - Orchestrates compilation

## Known Issues

- **Nix Sandbox**: On systems without kernel namespace support, you may need to use `--no-sandbox` when building
- Type checker and code generator are not yet implemented
- Integration tests will be skipped until code generation is complete

## Comparison with OCaml Implementation

| Feature | OCaml | Rust |
|---------|-------|------|
| LLVM Bindings | ocaml-llvm | Inkwell |
| Build System | Dune | Cargo |
| Parser | Menhir (LALR) | Hand-written recursive descent |
| Memory Safety | GC | Ownership |
| Pattern Matching | Excellent | Good |
| Error Messages | Good | Excellent |
| Status | ✅ Complete | 🚧 In Progress |

## License

MIT (or your preferred license)
