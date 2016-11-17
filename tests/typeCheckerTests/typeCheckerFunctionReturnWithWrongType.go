(* Fails - function has a return type but has return of another type *)

func fail() int {
  return false
}

{
  return 0 (* Necessary to have at least a valid statement in the body for the parser to pass *)
}
