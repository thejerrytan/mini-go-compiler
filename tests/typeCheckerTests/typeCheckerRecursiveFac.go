(* Recursive factorial function from project website- rewritten to fit our grammar rule that requires *)
(* a final return statement, without greater equal to or less than equal to - passes type checking *)

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
