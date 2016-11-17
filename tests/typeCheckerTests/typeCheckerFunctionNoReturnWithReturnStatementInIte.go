(* Fails - function has no return type but have return statement in ITE *)

func fail() {
  if true {
    return 0
  } else {
    return 0
  }
}

{
  return 0 (* Necessary to have at least a valid statement in the body for the parser to pass *)
}
