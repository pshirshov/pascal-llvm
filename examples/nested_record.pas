program NestedRecord;

type
  Inner = record
    value: Integer;
  end;

type
  Outer = record
    inner: Inner;
    x: Integer;
  end;

def test(): Integer = {
  var obj: Outer
  obj.inner.value = 42
  obj.x = 10
  return obj.inner.value + obj.x
}

def main(): Integer = {
  var result: Integer = test()
  writeln(result)
  return 0
}

.
