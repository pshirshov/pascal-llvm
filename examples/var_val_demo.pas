program VarValDemo;

function make_point(a: integer; b: integer): integer;
begin
  return a + b
end;

function main(): integer;
begin
  (* val declarations are immutable *)
  val pi: integer = 314;
  val radius: integer = 10;

  (* var declarations are mutable *)
  var counter: integer := 0;
  var x: integer := 5;

  (* Can read vals *)
  writeln(pi);
  writeln(radius);

  (* Can modify vars *)
  counter := counter + 1;
  writeln(counter);

  x := x + 10;
  writeln(x);

  (* Mixing var and val in expressions *)
  var sum: integer := pi + counter;
  writeln(sum);

  (* Use val in function call *)
  var result: integer := make_point(radius, x);
  writeln(result);

  return 0
end;

.
