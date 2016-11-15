open Go

let ( |> ) x f = f x (* pipeline function *)

let rec eqTy t1 t2 = match (t1, t2) with
  | (TyInt, TyInt) -> true
  | (TyBool, TyBool) -> true
  | (TyChan t1, TyChan t2) -> eqTy t1 t2
  | (TyFunc (ts1, t1), TyFunc (ts2, t2)) -> eqTy t1 t2 &&
                                            (List.length ts1 == List.length ts2) &&
                                            (List.for_all (fun (t1,t2) -> eqTy t1 t2) (List.combine ts1 ts2))

let lookup el lst = try (Some (snd (List.find (fun (el2,_) -> el = el2) lst))) with
                    | Not_found -> None

let rec inferTyExp env e = match e with
  | IConst i -> Some TyInt
  | BConst b -> Some TyBool
  | Var v -> (match (lookup v env) with
             | Some t -> Some t
             | None -> None)
  | And (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e1) with
                    | (Some t1, Some t2) -> if t1 = TyBool && t2 = TyBool
                                            then Some TyBool
                                            else None (* Both must be TyBool *)
                    | _ -> None)
  | Eq (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if eqTy t1 t2
                                           then Some TyBool
                                           else None (* Both must be of same type *)
                   | _ -> None)
  | Gt (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if eqTy t1 t2
                                           then Some TyBool
                                           else None (* Both must be of same type *)
                   | _ -> None)
  | Plus (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                           then Some TyInt
                                           else None (* Both must be of TyInt *)
                   | _ -> None)
  | Minus (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                           then Some TyInt
                                           else None (* Both must be of TyInt *)
                   | _ -> None)
  | Times (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e1) with
                   | (Some t1, Some t2) -> if t1 = TyInt && t2 = TyInt
                                           then Some TyInt
                                           else None (* Both must be of TyInt *)
                   | _ -> None)
  | Division (e1, e2) -> (match (inferTyExp env e1, inferTyExp env e1) with
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
                                | TyFunc (tl, typeOption) -> Some t
                                | _ -> None) (* Must be TyFunc or else it is not a FuncExp *)
                      | None -> None)
  (* | SkipExp -> Some env *) (* why did we comment this out? *)

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
  | Decl (v, e) -> let r = inferTyExp env e in
                   (match r with
                   | None -> None
                   | Some t -> Some (update (v, t) env)) (* Update the environment with type of expression e to variable v *)
  | Return e -> (match inferTyExp env e with
                | None -> None
                | Some t -> Some env)
  | Print e -> (match inferTyExp env e with
               | None -> None
               | Some t -> Some env)
  | While (e, s) -> let r = inferTyExp env e in
                  (match r with
                  | None -> None
                  | Some t -> if t = TyBool
                              then (match typeCheckStmt env s with
                                   | (Some env1) -> Some env
                                   | None -> None)
                              else None) (* the while statement check is not of TyBool *)
  | ITE (e, s1, s2) -> let r = inferTyExp env e in
                    (match r with
                    | None -> None
                    | Some t -> if t = TyBool
                                then (match (typeCheckStmt env s1, typeCheckStmt env s2) with
                                     | (Some env1, Some env2) -> Some env
                                     | _ -> None) (* Both s1 and s2 must pass *)
                                else None) (* the if statement check is not of TyBool *)
  | Seq (s1, s2) -> (match (typeCheckStmt env s1, typeCheckStmt env s2) with
                    | (Some env1, Some env2) -> Some env2
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
  | DeclChan v -> (match lookup v env with
                  | Some t -> (match t with
                            | TyChan t1 -> Some env
                            | _ -> None) (* Variable declared is not of type TyChan *)
                  | None -> None) (* No such variable declared *)
  | FuncCall (v, expl) -> (match lookup v env with
                          | Some t -> (match t with
                                    | TyFunc (tl, typeOption) -> Some env
                                    | _ -> None) (* variable declared if not of type TyFunc *)
                          | None -> None) (* No such variable declared *)
  | Skip -> Some env

  let rec updateEnvironmentWithExpressionTypeList etl = List.map (fun(vt) -> vt) etl

  let rec finalLineReturnType env s = match s with
    | Seq (s1, s2) -> finalLineReturnType env s2 (* Since we are checking the final line, we can ignore the first line unless it is already the return statement itself *)
    | Return e -> (match inferTyExp env e with
                  | None -> None (* Type inference of return type failed *)
                  | Some rt -> Some rt) (* We got a return type from type inference *)
    | _ -> None

  let rec isHaveReturnsInStatement s = match s with
    | Seq (s1, s2) -> (match isHaveReturnsInStatement s1 with
                      | Some s -> isHaveReturnsInStatement s2 (* There is no return statement in s1 so we check for the existence in s2 *)
                      | None -> None) (* If the first statement s1 has a return statement, we can fail here *)
    | Return e -> None (* Failure = None when there is a return statement detected *)
    | _ -> Some s (* For any other statement that is not a return, we can simply just say it's a pass with a Some *)

let rec typeCheckProc env proc = match proc with
  | (v, etl, ot, s) -> (match lookup env v with
                       | Some t -> let env = updateEnvironmentWithExpressionTypeList etl in
                                   (match t with
                                    | TyFunc (tl, typeOption) -> (match ot with
                                                            | Some t -> (match finalLineReturnType env s with
                                                                     | None -> None (* Func has return type, last line of function is not a return *)
                                                                     | Some rt -> if rt = t (* Func has return type, last line of function is a return and has same type *)
                                                                                  then Some env
                                                                                  else None)
                                                            | None -> (match isHaveReturnsInStatement s with
                                                                   | Some s -> Some env (* Func has no return type, function has no return statements  *)
                                                                   | None -> None))
                                    | _ -> None) (* not really necessary since we know that it is definitely some kind of TyFunc but this serves as an extra check *)
                       | None -> None)
  | _ -> None

let rec typeCheckProg env prog = match prog with
  | (procl, s) -> (match (procl |> List.map (fun proc -> typeCheckProc env proc) |> List.filter (fun ty -> ty = None) |> List.length, typeCheckStmt env s) with
                  | (0, Some t) -> Some env
                  | _ -> None)
