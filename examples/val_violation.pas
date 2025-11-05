program ValViolation;

function main(): integer;
begin
  val x: integer = 10;
  writeln(x);
  x := 20;  (* This should cause a type error *)
  writeln(x);
  return 0
end;

.
