open go

(* equality among types *)
let rec eqTy t1 t2 = match (t1,t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) -> eqTy t1 t2 &&
                                            (List.length ts1 == List.length ts2) &&
                                            (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))


(*
We assume that the type environment is represented as a list of pairs of variables and types
where variables are represented as strings.

Here's a convenience function which lookups if there's a binding for a variable.
It's actual type is actually slightly more general:
'a -> ('a * 'b) list -> 'b option

 *)
let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el = el2) lst))) with
                    | Not_found -> None


(* Implementation of G |- exp : t where we use 'option' to report failure *)
let rec inferTyExp env e = match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | Var v -> match (lookup v env) with
             | Some t -> Some t
             | None -> None
  | And (v1, v2) -> match (inferTyExp env v1, inferTyExp env v2) with
                    | (Some t1, Some t2) -> if t1 = TyBool && t2 && TyBool then Some TyBool else None
                    | _ -> None
  | Eq (v1, v2) -> match (inferTyExp env v1, inferTyExp env v2) with
                   | (Some t1, Some t2) -> if eqTy t1 t2 then Some TyBool else None
                   | _ -> None
  | Gt (v1, v2) -> match (inferTyExp env v1, inferTyExp env v2) with
                   | (Some t1, Some t2) -> if eqTy t1 t2 then Some TyBool else None
                   | _ -> None
  | Plus (v1, v2) -> match (inferTyExp env v1, inferTyExp env v2) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt then Some TyInt else None
                   | _ -> None
  | Minus (v1, v2) -> match (inferTyExp env v1, inferTyExp env v2) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt then Some TyInt else None
                   | _ -> None
  | Times (v1, v2) -> match (inferTyExp env v1, inferTyExp env v2) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt then Some TyInt else None
                   | _ -> None
  | Division (v1, v2) -> match (inferTyExp env v1, inferTyExp env v2) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt then Some TyInt else None
                   | _ -> None
  | Not v -> match inferTyExp env v with
             | Some t -> if t == TyBool then Some TyBool else None
             | None -> None
  | RcvExp v -> match (lookup v env) with (* unsure - what is RcvExp? *)
             | Some t -> Some t
             | None -> None
  | FuncExp (v1, v2) -> match lookup v1 env with (* unsure whether this is the correct idea, checking
                                                    if first one is TyFunc and ensuring that the rest
                                                    is not of TyFunc since we cannot pass and return functions? *)
                        | Some t1 -> if t1 = TyFunc
                                     then match v2 with
                                          | exp List l -> if List.exists TyFunc l
                                                          then None
                                                          else inferTyExp env List.nth List.length (l - 1) (* assume last line is the type of the TyFunc? *)
                                          | _ -> None
                                     else None
                        | None -> None

(* Implementation of G | (stmt : Cmd | G) where we simply skip Cmd and as above
  use 'option' to report failure

  As discussed, there's a bit of design space when it comes to 'nested' type declarations.
  The below is just a sketch.

*)
let rec typeCheckStmt env stmt = match stmt with
  | Assign (v,e) -> match (lookup v env) with
                    | None -> None (* Unknown variable *)
                    | Some t1 -> let t2 = inferTyExp env e in
                                 match t2 with
                                 | None -> None
                                 | Some t3 -> if eqTy t1 t3
                                              then Some env
                                              else None
  | Decl (v,e) -> let r = inferTyExp env v in
                  match r with
                  | None -> None
                  | Some t -> Some (update (v,t) env) (* update is not yet implemented *)

(*

1. We also have to take care of the error case which is only implicit in the above typing rule.

2. We assume some update function to extend the type binding.

Recall that we can redeclare variables in some nested scope.

Hence, update will override any earlier binding for v.

1. let rec update v t = func (env) -> (List.find (fun (v2, t1) -> v2 = v1) env)

aka ->

() ++ (x : int)         = (x : int)

(x : int) ++ (x : bool) = (x : bool)

*)

(*

What's still missing are implementations for

(1) collection of type signatures from functions (procedures)

(2) type checking of procedure bodies, and

(3) type checking of the main program.

 *)
