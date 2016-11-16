(* Correctly fail with no return type but have return statement *)

(* Correctly fail with no return type but have return statements in ITE and While *)

(* Correctly fail with return type but have no return statement *)

(* Correctly pass with no return type and no return statement *)

(* Correctly pass with return type and have return statement *)

(* Correctly fail with return type int and declare a with int and then return an expression of type int *)

(* Correctly fail with return type and return type of wrong expression in ITE and While *)

(* Correctly fail with parameters type and not being given right number of types *)

(* Correctly fail with paramters type and not being given the right kind of types *)

func ping(a int) int {
  return a
}

{
  a := true;
  f := !false;
  f := !a
  (* a := 1 *)
}
