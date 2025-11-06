program ArithmeticTest;

def compute(): Integer = {
  var a: Integer = 10
  var b: Integer = 5
  var c: Integer = (a + b) * 2 - 10
  return c
}

def main(): Integer = {
  var result: Integer = compute()
  writeln(result)
  return 0
}

.
