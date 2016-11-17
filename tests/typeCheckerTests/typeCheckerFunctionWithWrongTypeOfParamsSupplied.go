(* Fails - function needs an int parameter but is supplied a bool parameter *)

func fail(a int) {
  a := 1 (* dummy declaration so we don't have an awkward empty function *)
}

{
  fail(false)
}
