program FactorialDemo;

def factorial(n: Integer): Integer = {
  if (n <= 1) {
    return 1
  } else {
    return n * factorial(n - 1)
  }
}

def main(): Integer = {
  var value: Integer = factorial(5)
  writeln(value)
  return 0
}

.
