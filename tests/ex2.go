{
  abcd := newChannel;   (* DeclChan 'abcd' *)
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