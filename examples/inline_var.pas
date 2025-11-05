program InlineVar;

function main(): integer;
begin
  var x: integer := 10;
  var y: integer := 20;
  writeln(x + y);
  x := 15;
  writeln(x + y);
  return 0
end;

.
