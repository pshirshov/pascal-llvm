# Pascal-like Language Compiler

A simple Pascal-like language compiler built on LLVM using OCaml.

## Features

- **Data Types**: Integer, Real, Boolean, Char, String
- **Composite Types**: Arrays, Records (structs), Pointers
- **Control Flow**: if/then/else, while, for loops
- **Functions**: Functions and procedures with recursion support
- **I/O**: Console input/output via writeln, write, readln
- **Pointers**: Pointer arithmetic, address-of (@), dereference (^), dynamic allocation (new)

## Prerequisites

You need:
- OCaml >= 5.2
- Dune >= 3.0
- Menhir parser generator
- LLVM >= 18 with OCaml bindings

## Building

### Option 1: Using Nix (Recommended)

If you encounter sandboxing errors with nix, you may need to configure nix to disable sandboxing. Create or edit `~/.config/nix/nix.conf`:

```
sandbox = false
```

Then:

```bash
direnv allow
dune build
```

### Option 2: Manual Installation

Install dependencies via opam:

```bash
opam install dune menhir llvm
dune build
```

The executable will be at `_build/default/src/main.exe`.

## Usage

Compile a Pascal program:

```bash
dune exec pascalc examples/hello.pas
```

This generates an LLVM IR file (`hello.ll`). To compile to native code:

```bash
llc hello.ll -o hello.s
gcc hello.s -o hello
./hello
```

## Example Programs

- `examples/test2.pas` - Simple variable assignment and I/O
- `examples/fibonacci.pas` - **Recursive Fibonacci** - computes fib(5) = 5
- `examples/simple_func.pas` - Function with parameters
- `examples/if_test.pas` - Conditional statements

## Language Syntax

### Program Structure

```pascal
program ProgramName;

// Type declarations
type
  TypeName = TypeDefinition;

// Variable declarations
var
  variableName: TypeName;

// Function/Procedure declarations
function FunctionName(param: Type): ReturnType;
var
  localVar: Type;
begin
  // statements
  return value
end;

// Main program body
begin
  // statements
end.
```

### Types

- `integer` - 32-bit signed integer
- `real` - 64-bit floating point
- `boolean` - true or false
- `char` - single character
- `string` - string literal
- `^Type` - pointer to Type
- `array[size] of Type` - fixed-size array
- `record ... end` - struct/record type

### Statements

- Assignment: `variable := expression`
- Function call: `functionName(args)`
- If: `if condition then ... else ... end`
- While: `while condition do ... end`
- For: `for variable := start to stop do ... end`
- I/O: `writeln(...)`, `write(...)`, `readln(...)`
- Return: `return expression`

### Expressions

- Arithmetic: `+`, `-`, `*`, `/`, `mod`
- Comparison: `=`, `<>`, `<`, `<=`, `>`, `>=`
- Logical: `and`, `or`, `not`
- Array access: `array[index]`
- Record access: `record.field`
- Pointer: `@variable`, `pointer^`
- Allocation: `new(Type)`

## Implementation Details

The compiler consists of several stages:

1. **Lexer** (`src/lexer.mll`) - Tokenizes input
2. **Parser** (`src/parser.mly`) - Builds AST using Menhir
3. **Type Checker** (`src/types.ml`) - Validates types and semantics
4. **Code Generator** (`src/codegen.ml`) - Generates LLVM IR
5. **Main Driver** (`src/main.ml`) - Orchestrates compilation

The compiler uses LLVM OCaml bindings to generate efficient machine code.

## Usage Example

```bash
# Simple program
./compile.sh examples/test2.pas my_program
./my_program  # outputs: 42

# Recursive Fibonacci
./compile.sh examples/fibonacci.pas fibonacci
./fibonacci  # outputs: 5 (fib(5))
```

## Limitations

- No dynamic arrays
- No function pointers
- Limited string operations
- Record field access in codegen not fully implemented
- No separate compilation/modules
- Return statements directly in if/else branches require begin/end blocks
- Grammar requires all programs to end with just declarations (no main block)
