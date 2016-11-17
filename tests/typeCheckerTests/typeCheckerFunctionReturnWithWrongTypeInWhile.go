(* Fails - function has a return type but has return of another type in While *)

func fail() int {
  while true {
    return false
  };
  return 1 (* This is for the grammar where we need a final statement as return *)
}

{
  return 0 (* Necessary to have at least a valid statement in the body for the parser to pass *)
}
