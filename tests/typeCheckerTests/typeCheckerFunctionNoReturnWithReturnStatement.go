(* Fails - function has no return type but have return statement *)

func fail() {
  return 0
}

{
  return 0 (* Necessary to have at least a valid statement in the body for the parser to pass *)
}
