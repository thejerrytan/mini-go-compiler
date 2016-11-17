open Go

let rec eqTy t1 t2 = match (t1, t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, Some t1), TyFunc (ts2, Some t2)) -> eqTy t1 t2 &&
                                                      (List.length ts1 == List.length ts2) &&
                                                      (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))
  | (_, _) -> false

let lookup el lst = try (Some (snd (List.find (fun (el2, _) -> el = el2) lst))) with
                    | Not_found -> None

let rec compareList l1 l2 = match (l1, l2) with
  | ([], []) -> true
  | ([], _) -> false
  | (_, []) -> false
  | (x::xs, y::ys) -> x = y && compareList xs ys

let rec inferTyExp env e = match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | Var v -> (match (lookup v env) with
             | Some t -> Some t
             | None -> None)
  | And (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e2) with
                    | (Some t1, Some t2) -> if t1 = TyBool && t2 = TyBool
                                            then Some TyBool
                                            else None (* Both must be TyBool *)
                    | _ -> None)
  | Eq (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e2) with
                   | (Some t1, Some t2) -> if eqTy t1 t2
                                           then Some TyBool
                                           else None (* Both must be of same type *)
                   | _ -> None)
  | Gt (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e2) with
                   | (Some t1, Some t2) -> if eqTy t1 t2
                                           then Some TyBool
                                           else None (* Both must be of same type *)
                   | _ -> None)
  | Plus (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e2) with
                     | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                             then Some TyInt
                                             else None (* Both must be of TyInt *)
                     | _ -> None)
  | Minus (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e2) with
                      | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                              then Some TyInt
                                              else None (* Both must be of TyInt *)
                      | _ -> None)
  | Times (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e2) with
                      | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                              then Some TyInt
                                              else None (* Both must be of TyInt *)
                      | _ -> None)
  | Division (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e2) with
                         | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                                 then Some TyInt
                                                 else None (* Both must be of TyInt *)
                         | _ -> None)
  | Not e -> (match inferTyExp env e with
             | Some t -> if t = TyBool
                         then Some TyBool
                         else None (* Must be TyBool or Not does not make sense *)
             | None -> None)
  | RcvExp v -> (match (lookup v env) with
                | Some t -> (match t with
                            | TyChan t1 -> Some t
                            | _ -> None) (* Must be TyChan or RcvExp does not make sense *)
                | None -> None)
  | FuncExp (v, expl) -> (match lookup v env with
                         | Some t -> (match t with
                                     | TyFunc (tl, typeOption) -> (match compareList tl (List.map (fun x -> (match inferTyExp env x with
                                                                                                            | Some t -> t)) expl) with
                                                                  | true -> typeOption
                                                                  | _ -> None)
                                     | _ -> None) (* Must be TyFunc or else it is not a FuncExp *)
                         | None -> None)

let update vt env = vt :: (List.filter (fun (v1, t1) -> v1 <> fst vt) env)

let rec typeCheckStmt env stmt = match stmt with
  | Assign (v, e) -> (match (lookup v env) with
                     | None -> None
                     | Some t1 -> let t2 = inferTyExp env e in
                                  (match t2 with
                                  | None -> None
                                  | Some t3 -> if eqTy t1 t3
                                               then Some env
                                               else None)) (* type of variable v does not have the same type as the type assigned in e *)
  | Decl (it, v, e) -> let r = inferTyExp env e in
                       (match r with
                       | None -> None
                       | Some t -> Some (update (v, t) env))
  | Return e -> (match inferTyExp env e with
                | None -> None
                | Some t -> Some env)
  | Print e -> (match inferTyExp env e with
               | None -> None
               | Some t -> Some env)
  | While (e, (Locals (l), s)) -> let r = inferTyExp env e in
                         (match r with
                         | None -> None
                         | Some t -> if t = TyBool
                                     then (match typeCheckStmt env s with
                                           | (Some s1) -> Some env
                                           | None -> None)
                                     else None) (* the while statement check is not of TyBool *)
  | ITE (e, (Locals (l1), s1), (Locals (l2), s2)) -> let r = inferTyExp env e in
                                    (match r with
                                    | None -> None
                                    | Some t -> if t = TyBool
                                                then (match (typeCheckStmt env s1, typeCheckStmt env s2) with
                                                | (Some stmt1, Some stmt2) -> Some env
                                                | _ -> None) (* Both s1 and s2 must pass *)
                                                else None) (* the if statement check is not of TyBool *)
  | Seq (s1, s2) -> (match typeCheckStmt env s1 with
                    | Some env1 -> (match typeCheckStmt env1 s2 with
                                   | Some env2 -> Some env2
                                   | _ -> None)
                    | _ -> None) (* Both s1 and s2 must pass *)
  | Go s -> (match typeCheckStmt env s with
            | Some env1 -> Some env
            | None -> None)
  | RcvStmt v -> (match lookup v env with
                 | Some t -> (match t with
                          | TyChan t1 -> Some env
                          | _ -> None) (* Recieve statement must be passed a type of TyChan *)
                 | None -> None)
  | Transmit (v, e) -> (match lookup v env with
                       | Some t -> (match t with
                                | TyChan t1 -> (match inferTyExp env e with
                                            | Some t2 -> if t2 = TyInt
                                                         then Some env
                                                         else None (* Tranmission must be passed a type of TyInt *)
                                            | None -> None)
                                | _ -> None)
                       | None -> None)
  | DeclChan v -> Some (update (v, TyChan (TyInt)) env)
  | FuncCall (v, expl) -> (match lookup v env with
                          | Some t -> (match t with
                                    | TyFunc (tl, typeOption) -> (match compareList tl (List.map (fun x -> (match inferTyExp env x with
                                                                                                           | Some t -> t)) expl) with
                                                                 | true -> Some env
                                                                 | _ -> None)
                                    | _ -> None) (* variable declared if not of type TyFunc *)
                          | None -> None) (* No such variable declared *)
  | Skip -> Some env

let rec checkAllReturnType env s rt = match s with
  | Decl (it, v, e) -> let r = inferTyExp env e in
                       (match r with
                       | None -> None
                       | Some t -> Some (update (v, t) env))
  | Return e -> (match inferTyExp env e with
                | None -> None (* Type inference of return type failed *)
                | Some t -> if t = rt
                            then Some env
                            else None) (* We got a return type from type inference *)
  | Seq (s1, s2) -> (match checkAllReturnType env s1 rt with
                    | Some env -> (match checkAllReturnType env s2 rt with
                                  | Some env -> Some env
                                  | None -> None)
                    | _ -> None)
  | While (e, (Locals (l), s)) -> (match checkAllReturnType env s rt with
                                  | Some env -> Some env
                                  | None -> None)
  | ITE (e, (Locals (l1), s1), (Locals (l2), s2)) -> (match checkAllReturnType env s1 rt with
                                                     | Some env -> (match checkAllReturnType env s2 rt with
                                                                   | Some env -> Some env
                                                                   | None -> None)
                                                     | _ -> None)
  | DeclChan v -> Some (update (v, TyChan (TyInt)) env)
  | _ -> Some env

let rec isHaveReturnsInStatement s = match s with
  | Seq (s1, s2) -> (match isHaveReturnsInStatement s1 with
                    | Some s -> isHaveReturnsInStatement s2 (* There is no return statement in s1 so we check for the existence in s2 *)
                    | None -> None) (* If the first statement s1 has a return statement, we can fail here *)
  | Return e -> None (* Failure = None when there is a return statement detected *)
  | While (e, (Locals (l), s)) -> isHaveReturnsInStatement s
  | ITE (e, (Locals (l1), s1), (Locals (l2), s2)) -> (match (isHaveReturnsInStatement s1, isHaveReturnsInStatement s2) with
                                                     | (Some s1, Some s2) -> Some s1
                                                     | _ -> None)
  | _ -> Some s (* For any other statement that is not a return, we can simply just say it's a pass with a Some *)
  (* TODO: Write in readme as to why we didn't check for Go block returns *)
  (* TODO: Write in readme as to we allow for redeclaration but it might lead to unspecified behaviors *)

let extractTypes etl = List.map (fun(vt) -> match vt with
                                            (Var v, t) -> t) etl

let rec augmentStatement env stmt = match stmt with
  | Decl (it, v, e) -> let r = inferTyExp env e in
                       (match r with
                       | None -> None
                       | Some t -> Some (Decl (Some (t), v, e)))
  | Seq (s1, s2) -> (match (augmentStatement env s1, augmentStatement env s2) with
                    | (Some stmt1, Some stmt2) -> Some (Seq (stmt1, stmt2))
                    | _ -> None)
  | Go s -> (match augmentStatement env s with
            | Some s1 -> Some (Go (s1))
            | None -> None)
  | ITE (e, (Locals (l1), s1), (Locals (l2), s2)) -> (match (augmentStatement env s1, augmentStatement env s2) with
                                                     | (Some stmt1, Some stmt2) -> Some (ITE (e, (Locals (l1), stmt1), (Locals (l2), stmt2)))
                                                     | _ -> None)
  | While (e, (Locals (l), s)) -> (match augmentStatement env s with
                                  | Some stmt1 -> Some (While (e, (Locals (l), stmt1)))
                                  | _ -> None)
  | _ -> Some stmt

let rec typeCheckProc env proc = match proc with
  | Proc (v, etl, ot, (Locals (l), s)) -> (let Some s = augmentStatement l s in match ot with
                                          | Some t -> (match checkAllReturnType l s t with
                                                      | None -> None (* Func has return type, not all return statements are of correct type *)
                                                      | Some env -> Some (v, TyFunc (extractTypes etl, ot))) (* Func has return type, all returns has same type *)
                                          | None -> (match isHaveReturnsInStatement s with
                                                    | Some s -> Some (v, TyFunc (extractTypes etl, ot)) (* Func has no return type, function has no return statements *)
                                                    | None -> None))

(* used to help convert exp * types list into string * types list required for the environment as in locals *)
let updateEnvironmentWithExpressionTypeList etl = List.map (fun(vt) -> match vt with
                                                                       | (Var v, t) -> (v, t)) etl

(* used to help make each statement know the locals  *)
let rec makeLocalsKnown env stmt = match stmt with
  | Decl (it, v, e) -> let r = inferTyExp env e in
                       (match r with
                       | None -> None
                       | Some t -> Some ((update (v, t) env), stmt))
  | While (e, (Locals (l), s)) -> (match makeLocalsKnown env s with
                                   | Some (env1, s1) -> Some (env, While (e, (Locals env1, s)))
                                   | None -> None)
  | ITE (e, (Locals (l1), s1), (Locals (l2), s2)) -> (match makeLocalsKnown env s1 with
                                                      | Some (env1, s3) -> (match makeLocalsKnown env s2 with
                                                                           | Some (env2, s4) -> Some (env, ITE (e, (Locals env1, s1), (Locals env2, s2)))
                                                                           | None -> None)
                                                      | None -> None)
  | Seq (s1, s2) -> (match makeLocalsKnown env s1 with
                    | Some (env1, s3) -> (match makeLocalsKnown env1 s2 with
                                         | Some (env2, s4) -> Some (env, Seq (s3, s4))
                                         | None -> None)
                    | None -> None)
  | Go s -> (match makeLocalsKnown env s with
            | Some (env, s) -> Some (env, s)
            | None -> None)
  | DeclChan v -> Some (update (v, TyChan (TyInt)) env, stmt)
  | _ -> Some (env, stmt)

let augmentProc env proc = match proc with
  | Proc (v, etl, ot, ls) -> let l1 = updateEnvironmentWithExpressionTypeList etl in
                             match ls with
                             | (Locals (l), s) -> (match augmentStatement l1 s with
                                                  | Some s1 -> (match makeLocalsKnown (env @ l1) s1 with
                                                               | Some (env1, s2) -> Proc (v, etl, ot, (Locals (env1), s2))))
                             (* we assume that before we augment any proc,
                             the initial state of locals is an empty list so we ignore it *)

let removeOptionForEnv env = List.map (fun pair -> match pair with
  | Some (a, b) -> (a, b)) (List.filter (fun something -> something <> None) env)

let prototypeEnvFromProcl procl = List.map (fun proc -> match proc with
                                                        | Proc (v, etl, ot, (Locals (l), s)) -> Some (v, TyFunc (extractTypes etl, ot))) procl

let filterTyFuncAnotherHelper lst = List.filter (fun y -> (match snd y with
                                                           | TyFunc (ts1, Some t1) -> false
                                                           | _ -> true)) lst

let rec filterTyFuncHelper s = (match s with
  | While (e, (Locals (l), s)) -> While (e, (Locals (filterTyFuncAnotherHelper l), filterTyFuncHelper s))
  | ITE (e, (Locals (l1), s1), (Locals (l2), s2)) -> ITE (e, (Locals (filterTyFuncAnotherHelper l1), filterTyFuncHelper s1), (Locals (filterTyFuncAnotherHelper l2), filterTyFuncHelper s2))
  | Seq (s1, s2) -> Seq (filterTyFuncHelper s1, filterTyFuncHelper s2)
  | Go s -> Go (filterTyFuncHelper s)
  | _ -> s)

let rec filterTyFuncs procl = List.map (fun x -> match x with
                                                 | Proc (v, etl, ot, (Locals (l), s)) -> Proc (v, etl, ot, (Locals (filterTyFuncAnotherHelper l), filterTyFuncHelper s))) procl

let rec typeCheckProg env prog = match prog with
  | Prog (procl, s) -> let prototypeEnv = prototypeEnvFromProcl procl in
                         if List.length (List.filter (fun x -> x = None) prototypeEnv) <> 0 then None else
                           let prototypeEnvi = List.map (fun something -> match something with Some (s, t) -> (s, t)) prototypeEnv in
                             let augmentedProcl = List.map (fun proc -> augmentProc prototypeEnvi proc) procl in (* adding locals into each proc *)
                               let env1 = List.map (fun proc -> typeCheckProc prototypeEnv proc) augmentedProcl in (* making sure that proc's return type is similar with the statement *)
                                 let failures = List.filter (fun ty -> ty = None) env1 in
                                   let numberOfFailures = List.length failures in
                                     if numberOfFailures <> 0 then None else
                                       let typeCheckedStatements = typeCheckStmt (List.map (fun something -> match something with Some (s, t) -> (s, t)) env1) s in
                                         let filteredAugmentedProcl = filterTyFuncs augmentedProcl in
                                           match (numberOfFailures, typeCheckedStatements) with
                                           | (0, Some env) -> (match augmentStatement env s with
                                                              | Some s -> (match makeLocalsKnown env s with
                                                                          | Some (env, s) -> Some (Prog (filteredAugmentedProcl, s))
                                                                          | None -> None)
                                                              | None -> None)
                                           | _ -> None
