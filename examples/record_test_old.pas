program RecordTest;

type
  Point = record
    x: integer;
    y: integer;
  end;

function test(): integer;
var
  p: Point;
begin
  p.x := 10;
  p.y := 20;
  return p.x + p.y
end;

function main(): integer;
var
  result: integer;
begin
  result := test();
  writeln(result);
  return 0
end;

.
