(* Recursive ping and pong functions by us - passes type checking *)

func ping(x int) int {
  if x == 0 {
    return 0
  } else {
    return x + pong(x - 1)
  };
  return 0
}

func pong(x int) int {
  if x == 0 {
    return 0
  } else {
    return x + ping(x - 1)
  };
  return 0
}

{
  x := ping(4);
  y := pong(4);
  x = pong(4);
  y = ping(4);
  print x;
  print y;
  print x + y
}
