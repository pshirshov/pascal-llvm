# Inline Variable Declarations

## Overview

The compiler now supports Scala-style inline variable declarations with `var` and `val` keywords directly in statement blocks, eliminating the need for forward declarations in many cases.

## Syntax

### Mutable Variables (var)
```pascal
var name: Type := initializer
```

### Immutable Variables (val)
```pascal
val name: Type = initializer
```

## Features

### 1. Inline Declarations
Variables can now be declared at the point of use:

```pascal
function main(): integer;
begin
  var x: integer := 10;
  val pi: integer = 314;

  x := x + 5;     (* OK - x is mutable *)
  (* pi := 100;   Would cause error - pi is immutable *)

  writeln(x);
  return 0
end;
```

### 2. Immutability Enforcement
The compiler enforces single-assignment for `val` declarations:

```pascal
val x: integer = 10;
x := 20;  (* Type error: Cannot assign to val: x *)
```

### 3. Type Safety
All inline declarations are fully type-checked with the same rules as forward declarations:

```pascal
var x: integer := 10;
var y: integer := 20 + x;
val sum: integer = x + y;
```

### 4. Works with All Types
Inline declarations work with all existing types:

```pascal
type Point = record
  x: integer;
  y: integer;
end;

function main(): integer;
begin
  var p: Point := make_point(10, 20);
  val origin_x: integer = 0;
  return 0
end;
```

## Implementation Details

### AST Changes
- Added `SVarDecl of identifier * type_expr * expr`
- Added `SValDecl of identifier * type_expr * expr`

### Type Checker
- Added `vals` hashtable to symbol table to track immutable variables
- Check assignments to prevent modification of vals
- Inline declarations add variables to local scope immediately

### Code Generator
- Inline declarations generate `alloca` + `store` at statement position
- Same LLVM IR generation as forward declarations

## Examples

### Example 1: Simple Usage
```pascal
function main(): integer;
begin
  var counter: integer := 0;
  val max: integer = 100;

  while counter < max do begin
    counter := counter + 1;
    writeln(counter)
  end;

  return 0
end;
```

### Example 2: Mixing Styles
Both forward declarations and inline declarations can be used together:

```pascal
function compute(): integer;
var
  total: integer;  (* Forward declaration *)
begin
  total := 0;

  var i: integer := 1;      (* Inline mutable *)
  val limit: integer = 10;   (* Inline immutable *)

  for i := 1 to limit do
    total := total + i;

  return total
end;
```

## Benefits

1. **Locality**: Variables can be declared close to their use
2. **Immutability**: `val` provides compile-time immutability guarantees
3. **Clarity**: Intent is clear - `var` signals mutability, `val` signals constants
4. **Type Safety**: Full type checking at declaration point

## Testing

All inline declaration features are covered by automated tests:
- `examples/inline_var.pas` - Mutable variables
- `examples/inline_val.pas` - Immutable variables
- `examples/val_violation.pas` - Immutability enforcement (compilation fails as expected)
- `examples/var_val_demo.pas` - Comprehensive demonstration
