(* Simple mini-go program with only the main body and statements with valid parsing result - pass *)

{
  a := true;              (* Decl bool *)
  b := false;             (* Decl bool *)
  c := 50;                (* Decl int double digit *)
  d := 1;                 (* Decl int single digit *)
  e := false;             (* Decl bool *)
  e = !a;                 (* Not bool *)
  e = (a == b);           (* Parenthesis, Eq *)
  e = (a && b);           (* And *)
  e = (a && b == e);      (* And vs Eq operator precedence *)
  e = (a && b) == e;      (* Associativity *)

  g := <- f1;             (* RcvExp var *)
  g = <- f;               (* RcvExp name *)

  h := 0;                 (* Decl *)
  h := c + d;             (* Plus *)
  h := c - d;             (* Minus *)
  h = c * d;              (* Times *)
  h = 100 * c;            (* Literal times Var *)
  h = d / c;              (* Divide *)
  h = c + d - c;          (* Left-Right associativity *)
  h = c * d / c;          (* Left-Right associativity *)
  h = c + d / c;          (* + / operator precedence *)
  h = c + d * c;          (* + x operator precedence *)
  h = c - d / c;          (* - / operator precedence *)
  h = c - d * c;          (* - x operator precedence *)
  h = (c + d) / (c - d);  (* Parenthesis associativity *)
  h = (c + d) * (c - d);  (* Parenthesis associativty *)

  return (h * 0)          (* Return bexp *)
}
