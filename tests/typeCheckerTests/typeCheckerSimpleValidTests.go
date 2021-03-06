(* Simple mini-go program with only the main body and valid usages - pass *)

{
  a := true;          (* Decl bool true *)
  b := false;         (* Decl bool false *)
  c := 50;            (* Decl int double digit *)
  d := 1;             (* Decl int single digit *)
  e := false;         (* Decl bool *)

  e = !a;             (* Assign Not bool *)
  f := !false;        (* Decl Not bool false *)
  f := !a;            (* ReDecl not bool var *)
  e = a == b;         (* Assign Equality between 2 bools *)
  e = a && b;         (* And between 2 bools *)
  e = a && b == e;    (* And between 2 bools equals between 2 bools *)

  a := 1;             (* ReDecl of bool var into int var after using it to decalre f as not of bool var *)

  abcd := newChannel; (* DeclChan var *)
  j := newChannel;    (* DeclChan name *)

  <- abcd;            (* RcvStmt var *)
  abcd <- 1;          (* Transmit *)

  g := <- abcd;       (* Decl, RcvExp var *)
  g = <- j;           (* RcvExp name *)

  h := 0;             (* Decl int Literal *)
  h := c + d;         (* ReDecl int var Plus int var *)
  h := c - d;         (* ReDecl int var Minus int var *)
  h = c * d;          (* Assign int var Times int var *)
  h = 100 * c;        (* Assign int Literal times int var *)
  h = d / c;          (* Assign int var Divide int var *)

  go {                (* Go *)
    b := true;        (* Decl *)
    while (b) {       (* While *)
      print(1 + 1)    (* Print *)
    }
  };

  return 1            (* Return int *)
}
