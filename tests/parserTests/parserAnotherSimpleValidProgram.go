(* Another simple mini-go program with only the main body and statements with valid parsing result - pass *)

{
  abcd := newChannel;   (* DeclChan *)
  <- abcd;              (* RcvStmt *)
  c := <- abcd;         (* Decl, RcvExp *)
  abcd <- 1;            (* Transmit *)
  go {                  (* Go block *)
    b := true;          (* Decl *)
    while (b) {         (* While bexp block *)
      print(1+1)        (* Print bexp *)
    }
  };

  if (false) {          (* ITE *)
    sum(2 * c)          (* FuncCall arg *)
  } else {
    c1 = 10;            (* Assign *)
    sum()               (* FuncCall [] *)
  };

  return a              (* Return bexp *)
}
