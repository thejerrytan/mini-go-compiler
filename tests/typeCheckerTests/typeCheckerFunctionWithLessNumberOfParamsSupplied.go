(* Fails - function needs 2 parameters but is called with 1 *)

func fail(a int, b int) {
  a := 1 (* dummy declaration so we don't have an awkward empty function *)
}

{
  fail(1)
}
