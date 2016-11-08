open Go

(* General question - When do we update the environment during the type checking process? *)

let ( |> ) x f = f x (* pipeline function *)

let rec eqTy t1 t2 = match (t1, t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) -> eqTy t1 t2 &&
                                            (List.length ts1 == List.length ts2) &&
                                            (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
  | (_, _) -> false

let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el = el2) lst))) with
                    | Not_found -> None

let rec inferTyExp env e = match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | Var v -> match (lookup v env) with
             | Some t -> Some t
             | None -> None
  | And (e1, e2) -> match (inferTyExp env e1, inferTyExp env e1) with
                    | (Some t1, Some t2) -> if t1 = TyBool && t2 && TyBool
                                            then Some TyBool
                                            else None
                    | _ -> None
  | Eq (e1, e2) -> match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if eqTy t1 t2
                                           then Some TyBool
                                           else None
                   | _ -> None
  | Gt (e1, e2) -> match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if eqTy t1 t2
                                           then Some TyBool
                                           else None
                   | _ -> None
  | Plus (e1, e2) -> match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                           then Some TyInt
                                           else None
                   | _ -> None
  | Minus (e1, e2) -> match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                           then Some TyInt
                                           else None
                   | _ -> None
  | Times (e1, e2) -> match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                           then Some TyInt
                                           else None
                   | _ -> None
  | Division (e1, e2) -> match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                           then Some TyInt
                                           else None
                   | _ -> None
  | Not e -> match inferTyExp env e with
             | Some t -> if t = TyBool
                         then Some TyBool
                         else None
             | None -> None
  | RcvExp v -> match (lookup v env) with
             | Some t -> if t = TyChan
                         then Some env
                         else None
             | None -> None
  | FuncExp (v, expl) -> match lookup v env with
                         | Some t -> if t = TyFunc
                                     then match expl |> List.map (fun e -> inferTyExp env e) |> List.filter (fun ty -> ty <> TyBool || ty <> TyInt) |> List.length with
                                          | 0 -> Some TyFunc (* To be confirmed, we might need to return the return type of the function *)
                                          | _ -> None
                                     else None
                         | None -> None

let rec typeCheckStmt env stmt = match stmt with
  | Assign (v, e) -> match (lookup v env) with
                     | None -> None
                     | Some t1 -> let t2 = inferTyExp env e in
                                  match t2 with
                                  | None -> None
                                  | Some t3 -> if eqTy t1 t3
                                               then Some env
                                               else None
  | Decl (v, e) -> let r = inferTyExp env e in
                   match r with
                   | None -> None
                   | Some t -> Some (update (v, t) env)
  | Return e -> match inferTyExp env e with
                | None -> None
                | Some t -> Some env
  | Print e -> match inferTyExp env e with
               | None -> None
               | Some t -> Some env
  | While (e, s) -> let r = inferTyExp env e in
                  match r with
                  | None -> None
                  | Some t -> if t = TyBool
                              then match typeCheckStmt env s with
                                   | (Some env1) -> Some env
                                   | None -> None
                              else None
  | ITE (e, s1, s2) -> let r = inferTyExp env e in
                    match r with
                    | None -> None
                    | Some t -> if t = TyBool
                                then match (typeCheckStmt env s1, typeCheckStmt env s2) with
                                     | (Some env1, Some env2) -> Some env
                                     | _ -> None
                                else None
  | Seq (s1, s2) -> match (typeCheckStmt env s1, typeCheckStmt env s2) with
                    | (Some env1, Some env2) -> Some env2
                    | _ -> None
  | Go s -> match typeCheckStmt env s with
            | Some env1 -> Some env
            | None -> None
  | RcvStmt v -> match lookup v env with
                 | Some t -> if t = TyChan
                             then Some env
                             else None
                 | None -> None
  | Transmit (v, e) -> match lookup v env with
                       | Some t -> if t = TyChan
                                   then match inferTyExp env e with
                                        | Some t1 -> if t1 = TyInt
                                                     then Some env
                                                     else None
                                        | None -> None
                                   else None
                       | None -> None
  | DeclChan v -> match lookup v env with
                  | Some t -> if t = TyChan
                              then Some env
                              else None
                  | None -> None
  | FuncCall (v, expl) -> match lookup v env with
                          | Some t -> if t = TyFunc
                                      then match expl |> List.map (fun e -> inferTyExp env e) |> List.filter (fun ty -> ty <> TyBool || ty <> TyInt) |> List.length with
                                           | 0 -> Some TyFunc (* To be confirmed, we might need to return the return type of the function *)
                                           | _ -> None
                                      else None
                          | None -> None
  | Skip -> Some env

let update vt env = vt :: (List.filter (fun (v1, t1) -> v1 <> fst vt) env)

let rec typeCheckProc env proc = match proc with
  | (v, etl, ot, s) -> match lookup env v with
                       | Some t -> if t = TyFunc (* This looks a little dubious, we seem to need to drop the expressions? *)
                                   then match etl |> List.map (fun (e, ty) -> ty) |> List.filter (fun ty -> ty <> TyBool || ty <> TyInt) |> List.length with
                                        | 0 -> match ot with
                                               | Some t -> match typeCheckStmt env s with
                                                           | None (* stmt has no return(s) *) -> None
                                                           | Some u(* stmt has return(s) *) -> Some u (* make sure all return stmt are of type t -> Some env else -> none*)
                                               | None -> match typeCheckStmt env s with
                                                         | None (* stmt has no return(s) *) -> Some env
                                                         | Some u(* stmt has return(s) *) -> Some u (* make sure all return stmt are of same type -> Some env else -> None *)
                                        | _ -> None
                                   else None
                       | None -> None
  | _ -> None

let rec typeCheckProg env prog = match prog with (* To be implemented: if the procedure is not None, we should augment the environment *)
  | (procl, s) -> match (procl |> List.map (fun proc -> typeCheckProc env proc) |> List.filter (fun ty -> ty = None) |> List.length, typeCheckStmt env s) with
                  | (0, Some t) -> Some env
                  | _ -> None
