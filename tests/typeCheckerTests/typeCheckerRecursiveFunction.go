func fac(x int) int {
  if x == 0 {
    return 1
  } else {
    if x == 1 {
      return 1
    } else {
      return x * fac(x - 1)
    }
  };
  return 1
}

{
  x := fac(4);
  print x
}
