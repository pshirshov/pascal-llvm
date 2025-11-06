program RecordLocal;

type
  Point = record
    x: Integer;
    y: Integer;
  end;

function test(): Integer;
var
  p: Point;
begin
  p.x := 10;
  p.y := 20;
  return p.x + p.y
end;

def main(): Integer = {
  var result: Integer = test()
  writeln(result)
  return 0
}

.
