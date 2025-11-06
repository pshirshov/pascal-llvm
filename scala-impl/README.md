# Pascal-like Compiler - Scala Implementation

A compiler for a Pascal-like language implemented in Scala 3, using fastparse for parsing and JavaCPP LLVM bindings for code generation.

## Status

✅ **Complete and tested** - All 10 test cases passing

## Features

- **Parser**: Fastparse-based parser combinators for clean, declarative parsing
- **Type Checker**: Strong static type checking with support for inference
- **Code Generator**: LLVM IR generation via JavaCPP bindings
- **Target**: Native executables via LLVM

### Recent Improvements

- **Parser fix**: Added support for optional trailing semicolons in record field declarations
- **LLVM codegen**: Fixed duplicate function declarations in LLVM IR generation
- **Testing**: Comprehensive test suite with scala-cli compatibility

## Technology Stack

- **Language**: Scala 3.5.2
- **Parser**: fastparse 3.1.1 (parser combinators)
- **LLVM Bindings**: JavaCPP llvm-platform 20.1.7
- **Build System**: SBT 1.10.5
- **Assembly**: sbt-assembly for fat JAR creation

## Project Structure

```
scala-impl/
├── build.sbt                      # SBT build configuration
├── project/
│   ├── build.properties           # SBT version
│   └── plugins.sbt                # SBT plugins
├── flake.nix                      # Nix development environment
├── src/main/scala/pascal/
│   ├── AST.scala                  # Abstract Syntax Tree definitions
│   ├── Parser.scala               # Fastparse-based parser
│   ├── TypeChecker.scala          # Type checking and validation
│   ├── CodeGen.scala              # LLVM IR code generation
│   └── Main.scala                 # CLI entry point
├── compile.sh                     # Compilation script
└── test.sh                        # Test runner
```

## Building

### Using Nix (Recommended)

```bash
cd scala-impl
nix develop
sbt assembly
```

### Manual Build

Requirements:
- JDK 21
- SBT 1.10.5
- LLVM 18
- GCC (for linking)

```bash
sbt assembly
```

## Usage

### Compile a Pascal Program

```bash
./compile.sh examples/fibonacci.pas fibonacci
./fibonacci
```

### Run Tests

```bash
./test.sh
```

## Implementation Details

### Parser

The parser uses fastparse, a powerful parser combinator library for Scala. Key features:

- **Declarative syntax**: Parsers composed from simple building blocks
- **Strong typing**: Type-safe AST construction
- **Error messages**: Good error reporting out of the box
- **Performance**: Fast parsing with minimal overhead

Example parser combinator:
```scala
def varDecl[$: P]: P[VarDecl] = P(
  identifier ~ ws ~ ":" ~ ws ~ typeExpr ~ (ws ~ ":=" ~ ws ~ expr).?
).map((name, tpe, init) => VarDecl(name, tpe, init))
```

### Type Checker

- Symbol table-based type checking
- Support for:
  - Type inference for expressions
  - Mutable (var) and immutable (val) variables
  - Function/procedure type checking
  - Record and array types
  - Pointer types

### Code Generator

Uses JavaCPP LLVM bindings to generate LLVM IR:

- Direct LLVM C API access via JavaCPP
- Support for all Pascal language features
- Optimized IR generation
- Native code via LLVM backend

## Comparison with Other Implementations

| Feature | Scala | OCaml | Rust |
|---------|-------|-------|------|
| Parser | fastparse (combinators) | Menhir (LALR) | Hand-written recursive descent |
| LLVM Bindings | JavaCPP (LLVM 20) | ocaml-llvm (LLVM 21) | Inkwell (LLVM 18) |
| Type System | Strong, functional | Strong, functional | Strong, ownership-based |
| Build System | SBT | Dune | Cargo |
| Lines of Code | ~1000 | ~1200 | ~1500 |

## Development

### Code Style

This implementation follows Scala 3 best practices:
- Enum types for ADTs
- Pattern matching
- Immutability where possible
- Functional programming style

### Testing

10 comprehensive tests covering:
- Variable declarations
- Recursive functions (fibonacci, factorial)
- Loops and control flow
- Record types
- Inline declarations (var/val)

### Alternative Testing with scala-cli

If you encounter issues with Nix or sbt, you can test the parser directly using scala-cli:

```bash
# Test parser on actual Pascal files
scala-cli run test_actual_file.scala

# Run specific parser tests
scala-cli run test_minimal.scala
scala-cli run test_exact.scala
```

### Known Issues

- **Nix sandbox**: On some systems without kernel namespace support, you may need to disable Nix sandboxing:
  ```bash
  nix develop --option sandbox false
  ```
- **JavaCPP cache**: The LLVM native libraries are cached in `~/.javacpp/cache/` on first run

## License

MIT
