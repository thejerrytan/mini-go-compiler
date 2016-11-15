{
  a := true;       (* Decl bool true *)
  b := false;      (* Decl bool false *)
  c := 50;         (* Decl int double digit *)
  d := 1;          (* Decl int single digit *)
  e := false;      (* Decl bool *)

  (* TODO: Add some failing tests cases that can be added *)

  e = !a;          (* Assign Not bool *)
  f := !false;     (* Decl Not bool false *)
  (* TODO: This is failing f := !a; ReDecl not bool var *)
  e = a == b;      (* Assign Equality between 2 bools *)
  e = a && b;      (* And between 2 bools *)
  e = a && b == e; (* And between 2 bools equals between 2 bools *)

  (* TODO: Add some failing tests cases that can be added *)

  a := 1;          (* ReDecl int single digit *)

  (* TODO: Add some failing tests cases that can be added *)

  (* TODO: This is failing g := <- f1; RcvExp var *)
  (* TODO: This is failing g = <- f; RcvExp name *)

  (* TODO: Add some failing tests cases that can be added *)

  h := 0;                  (* Decl int Literal *)
  h := c + d;              (* ReDecl int var Plus int var *)
  h := c - d;              (* ReDecl int var Minus int var *)
  h = c * d;               (* Assign int var Times int var *)
  h = 100 * c;             (* Assign int Literal times int var *)
  h = d / c;               (* Assign int var Divide int var *)

  (* TODO: Add some failing tests cases that can be added *)

  return 1;                 (* Return int *)
  return false;             (* Return bool Literal *)
  return true && false;     (* Return bool Literal And bool Literal *)
  return c + d              (* Return int var + int var *)

  (* TODO: Add some failing tests cases that can be added *)
}
