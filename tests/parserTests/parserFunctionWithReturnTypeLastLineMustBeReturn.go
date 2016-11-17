(* the last statement of a function with a return type must be a return statement - fail *)

func fail() int {
  a := 1;
  if true {
    b := 2
  } else {
    c := 3
  };
  print 123
}

{
  print 1
}
