(* Normalize.ml *)
open Go

let nameSupply = ref 0
let freshName _ =  nameSupply := !nameSupply + 1;
                   String.concat "" ["temp" ; string_of_int (!nameSupply )] 
let rec lookup (x: string) (xs : (string * Go.types) list) = 
  match List.filter (fun el -> (fst el) = x) xs with
  | [(n, t)] -> Some (n, t)
  | _ -> None

let renameLocal locals tbl x = match locals with 
  | Locals(xs) -> match lookup x xs with
                  | Some (n, t) -> let newName = freshName() in
                                    Hashtbl.add tbl x newName;
                                    Locals((newName, t)::(List.filter (fun el -> fst el <> x) xs)), tbl
                  | None -> locals, tbl

let renameVar tbl x = try( Hashtbl.find tbl x) with Not_found -> x  (* x -> y mapping must exist first *)
let remapLocal locals tbl = match locals with
  | Locals(xs) -> let ys = List.map (fun el -> let newName = freshName() in 
                           Hashtbl.add tbl (fst el) newName; print_endline newName; el) xs in 
                  Locals(ys), tbl
let renameArgs locals tbl args = locals, tbl, List.map 
  (fun el -> match (fst el) with 
    | Var(x) -> let y = renameVar tbl x in Var(y), (snd el)
    | _ as rest -> rest , (snd el)
  ) args

let getFst (x,y,z) = x
let getSnd (x,y,z) = y
let getTrd (x,y,z) = z

let rec renameExp locals tbl e = match e with
  | Eq (e1, e2) -> let r1 = renameExp locals tbl e1 in
                   let r2 = renameExp (getFst r1) (getSnd r1) e2 in
                   (getFst r2, getSnd r2, Eq (getTrd r1, getTrd r2)) 
  | And (e1, e2) -> let r1 = renameExp locals tbl e1 in
                    let r2 = renameExp (getFst r1) (getSnd r1) e2 in
                    (getFst r2, getSnd r2, And (getTrd r1, getTrd r2))
  | Gt (e1, e2) -> let r1 = renameExp locals tbl e1 in
                   let r2 = renameExp (getFst r1) (getSnd r1) e2 in
                   (getFst r2, getSnd r2, Gt (getTrd r1, getTrd r2))
  | Plus (e1, e2) -> let r1 = renameExp locals tbl e1 in
                     let r2 = renameExp (getFst r1) (getSnd r1) e2 in
                     (getFst r2, getSnd r2, Plus (getTrd r1, getTrd r2))
  | Minus (e1, e2) -> let r1 = renameExp locals tbl e1 in
                      let r2 = renameExp (getFst r1) (getSnd r1) e2 in
                      (getFst r2, getSnd r2, Minus (getTrd r1, getTrd r2))
  | Times (e1, e2) -> let r1 = renameExp locals tbl e1 in
                      let r2 = renameExp (getFst r1) (getSnd r1) e2 in
                      (getFst r2, getSnd r2, Times (getTrd r1, getTrd r2))
  | Division (e1, e2) -> let r1 = renameExp locals tbl e1 in
                         let r2 = renameExp (getFst r1) (getSnd r1) e2 in
                         (getFst r2, getSnd r2, Division (getTrd r1, getTrd r2))
  | Not (e1) -> let r1 = renameExp locals tbl e1 in
                    (getFst r1, getSnd r1, Not (getTrd r1))
  | RcvExp (e1) -> let r = renameVar tbl e1 in
                   (locals, tbl, RcvExp(r))
  | IConst (i) ->  locals, tbl, IConst(i)
  | BConst (b) -> locals, tbl, BConst(b)
  | Var (v) -> locals, tbl, Var(renameVar tbl v)
  | FuncExp (x, es) -> let rs = List.map (renameExp locals tbl) es in
                       (getFst (List.hd rs), getSnd (List.hd rs), FuncExp (x, List.map getTrd rs))

let rec renameStmt lcls init_tbl s = 
  let locals, tbl = remapLocal lcls init_tbl in 
  match s with
  | Seq (s1, s2) -> let (locals_s1, tbl_s1, stmt1) = renameStmt locals tbl s1 in
                    let (locals_s2, tbl_s2, stmt2) = renameStmt locals_s1 tbl_s1 s2 in
                    locals_s2, tbl_s2, Seq(stmt1, stmt2)
  | Go (s1) -> let r = renameStmt locals tbl s1 in 
               (getFst r, getSnd r, Go(getTrd r))
  | Transmit (s1, e1) -> let r1 = renameVar tbl s1 in
                         let r2 = renameExp locals tbl e1 in
                         (getFst r2, getSnd r2, Transmit(r1, getTrd r2))
  | RcvStmt (s1) -> let r = renameVar tbl s1 in
                    (locals, tbl, RcvStmt (r))
  | Decl (t, s1, e1) -> let s2 = freshName() in Hashtbl.add tbl s1 s2;
                        let r1 = renameLocal locals tbl s1 in
                        let r2 = renameExp (fst r1) (snd r1) e1 in
                        (getFst r2, getSnd r2, Decl (t, renameVar (getSnd r2) s1, getTrd r2))
  | DeclChan (s1) -> let r1 = renameLocal locals tbl s1 in
                      (fst r1, snd r1, DeclChan(renameVar (snd r1) s1))
  | Assign (s1, e1) -> let r1 = renameExp locals tbl e1 in
                        (getFst r1, getSnd r1, Assign(renameVar (getSnd r1) s1, getTrd r1))
  | While (e1, (locals_s1, s1)) -> let r1 = renameExp locals tbl e1 in
                                   let r2 = renameStmt locals_s1 tbl s1 in
                                   (getFst r2, getSnd r2, While(getTrd r1, (getFst r2, getTrd r2)))
  | ITE (e1, (locals_s1, s1), (locals_s2, s2)) -> let r1 = renameExp locals tbl e1 in
                                                  let r2 = renameStmt locals_s1 (getSnd r1) s1 in
                                                  let r3 = renameStmt locals_s2 (getSnd r1) s2 in
                                                  (getFst r3, getSnd r3, ITE(getTrd r1, (getFst r2, getTrd r2), (getFst r3, getTrd r3)))
  | Return (e1) -> let r1 = renameExp locals tbl e1 in
                   (locals, getSnd r1, Return (getTrd r1))
  | FuncCall (s1, es) -> let r1 = List.map (renameExp locals tbl) es in
                         (getFst (List.hd r1), tbl, FuncCall(s1, List.map getTrd r1))
  | Print (e1) -> let r1 = renameExp locals tbl e1 in
                  (getFst r1, getSnd r1, Print(getTrd r1))
  | Skip -> (locals, tbl, Skip)


let rec normalizeExp e = match e with
  | Eq (e1, e2)  -> let r1 = normalizeExp e1 in
                    let r2 = normalizeExp e2 in
                    (Seq (fst r1, fst r2),
                     Eq (snd r1, snd r2))

  | And (e1, e2) -> let r1 = normalizeExp e1 in
                    let r2 = normalizeExp e2 in
                    (Seq (fst r1, fst r2),
                     And (snd r1, snd r2))

  | Gt (e1, e2) ->  let r1 = normalizeExp e1 in
                    let r2 = normalizeExp e2 in
                    (Seq (fst r1, fst r2),
                     Gt (snd r1, snd r2))                      
                      
  | Plus (e1, e2) -> let r1 = normalizeExp e1 in
                     let r2 = normalizeExp e2 in
                     (Seq (fst r1, fst r2),
                      Plus (snd r1, snd r2))

  | Minus (e1, e2) -> let r1 = normalizeExp e1 in
                      let r2 = normalizeExp e2 in
                      (Seq (fst r1, fst r2),
                       Minus (snd r1, snd r2))                       

  | Times (e1, e2) -> let r1 = normalizeExp e1 in
                      let r2 = normalizeExp e2 in
                      (Seq (fst r1, fst r2),
                       Times (snd r1, snd r2))

  | Division (e1, e2) -> let r1 = normalizeExp e1 in
                         let r2 = normalizeExp e2 in
                        (Seq (fst r1, fst r2),
                         Division (snd r1, snd r2))

  | Not e1            -> let r1 = normalizeExp e1 in
                         (fst r1,
                          Not (snd r1))

  | RcvExp ch         -> let x = freshName() in
                         (Decl (Some(TyChan TyInt), x, RcvExp ch),
                          Var x)

  (* Introduce Skip for convenience, maybe should have
     represented a sequence of commands as a list ... *)
                           
  | IConst i          -> (Skip, IConst i)
                           
  | BConst b          -> (Skip, BConst b)

  | Var x              -> (Skip, Var x)

  (* Need to normalize function arguments and make sure that order remains right *)
                            
  | FuncExp (x,es)     -> let rs = List.map normalizeExp es in
                          let c  = List.fold_left (fun a -> fun b -> Seq (a,b)) Skip (List.map fst rs) in 
                          let xs = List.map snd rs in
                          let y = freshName() in
                          (Seq (c, Decl (None, y, FuncExp (x,xs))),
                           Var y)

let rec normalizeStmt s = match s with
  | Seq (s1,s2) -> Seq (normalizeStmt s1, normalizeStmt s2)
  | Go s        -> Go (normalizeStmt s)
  | Transmit (x,e) -> let r = normalizeExp e in
                      Seq (fst r, Transmit (x, snd r))
  | RcvStmt x   -> RcvStmt x   
  | Decl (t,x,e)  -> let r = normalizeExp e in
                   Seq (fst r, Decl (t, x, snd r))
  | DeclChan x  -> DeclChan x
  | Assign (x,e) -> let r = normalizeExp e in
                    Seq (fst r, Assign (x, snd r))
  | While (e,(locals, s))  -> let r = normalizeExp e in
                    Seq (fst r, While (snd r,(locals, normalizeStmt s)))
  | ITE (e,(locals1, s1),(locals2, s2)) -> let r = normalizeExp e in
                     Seq (fst r, ITE (snd r, (locals1, normalizeStmt s1), (locals2, normalizeStmt s2)))
  | Return e      -> let r = normalizeExp e in
                     Seq (fst r, Return (snd r))
  | FuncCall (x, es) -> let rs = List.map normalizeExp es in
                        let c = List.fold_left (fun a -> fun b -> Seq (a,b)) Skip (List.map fst rs) in 
                        let xs = List.map snd rs in
                        Seq (c, FuncCall (x,xs))
  | Print e       -> let r = normalizeExp e in
                     Seq (fst r, Print (snd r))
  | Skip          -> Skip

let normalizeProc p = match p with
    Proc (x, args, tys, (locals,s)) -> Proc (x, args, tys, (locals, normalizeStmt s))

let normalizeProg p = match p with
    Prog (ps, s) -> Prog (List.map normalizeProc ps, normalizeStmt s)

let renameProc p = match p with
    Proc (x, args, tys, (locals, s)) -> let newLocals, newTbl, newS = renameStmt locals (Hashtbl.create 10) s in
                                        let nLocals, nTbl, newArgs = renameArgs newLocals newTbl args in
                                        Proc(x, newArgs, tys, (newLocals, newS))

let renameProg p = match p with
    Prog (ps, s) ->   let locals = Locals([]) in
                      let env = Hashtbl.create 10 in
                      let r1 = renameStmt locals env s in
                      Prog (List.map renameProc ps, getTrd r1)

