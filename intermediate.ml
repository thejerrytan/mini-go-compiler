(* Intermediate.ml *)
(* open Normalize *)
open Go

let tbl = Hashtbl.create 1000 (* hashtable to hold string variable names to memloc int mapping *)
let funcLocals = Hashtbl.create 100 (* Hashtable to hold func names to local variables list mapping *)
let nameSupply = ref (-1)
let freshName _ = nameSupply := !nameSupply + 1;
                !nameSupply (* here we let temp variables be int which allows us to do a direct mapping to int memloc later *)
let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
                    !labelSupply
let lookup var = try(Hashtbl.find tbl var) with
                  | Not_found -> print_endline ("[Intermediate] Cannot find var: " ^ var);1

type irc = IRC of (irc_cmd list)
[@@deriving show]

and irc_cmd = IRC_Assign of int * irc_exp
            | IRC_Label of int
            | IRC_Goto of int
            | IRC_NonzeroJump of int * int  (* if x L = if x non-zero then jump to L *)
            | IRC_Param of int (* Push param onto stack *)
            | IRC_Call of string * int (* (function name, number of parameters *)
            | IRC_Return of int (* Push return value onto stack? *)
            | IRC_Skip
            | IRC_Proc of (string list) * string * int (* procedure call of string at instruction label *)
(* Need another one for getting params from stack *)

and irc_exp = IRC_And of int * int
            | IRC_Eq of int * int
            | IRC_Gt of int * int
            | IRC_Plus of int * int
            | IRC_Minus of int * int
            | IRC_Times of int * int
            | IRC_Division of int * int
            | IRC_Not of int
            | IRC_IConst of int
            | IRC_Var of int                                    
            | IRC_Get (* Get from top of Env and assign to mem *)

(* short-hand for 'zero' jump *)
let irc_ZeroJump (x,l) = let y = freshName() in
                         [IRC_Assign (y, IRC_Not x);
                          IRC_NonzeroJump (y,l)]

(* (parts) of translation of Booleans (short-circuit evaluation!),
   yields a tuple where first component represents the IRC and
   second component a variable name which is bound to the result *)
let rec translateB exp = match exp with
  | BConst true -> let x = freshName() in
                   ([IRC_Assign (x, IRC_IConst 1)], x)
  | BConst false -> let x = freshName() in
                    ([IRC_Assign (x, IRC_IConst 0)], x)                      
  | And (e1, e2) -> (* 
                       e1.code;
                       if !e1.result goto l1
                       e2.code;
                       x = e2.result;
                       goto l2;
                       l1:
                       x = 0;             Booleans represented as integers
                       l2:
                     *)

                    let r1 = translateB e1 in
                    let r2 = translateB e2 in
                    let x = freshName() in
                    let l1 = freshLabel() in
                    let l2 = freshLabel() in                   
                    ((fst r1)
                     @
                     (irc_ZeroJump (snd r1,l1))
                     @
                     (fst r2)
                     @
                     [IRC_Assign (x, IRC_Var (snd r2));
                      IRC_Goto l2 ]                       
                     @
                     [IRC_Label l1;
                      IRC_Assign (x, IRC_IConst 0);
                      IRC_Label l2],
                     x)
| Eq (e1, e2) -> (* e1.code;
                 if e1.result goto l1
                 e2.code;
                 x = e2.result
                 goto l2
                 l1
                 x = 1
                 l2 *)
                let r1 = translateB e1 in
                let r2 = translateB e2 in
                let x = freshName() in
                let y = freshName() in
                let z = freshName() in
                ((fst r1)
                @ [IRC_Assign (x, IRC_Var (snd r1));
                   IRC_Assign (y, IRC_Var (snd r2));
                   IRC_Assign (z, IRC_Eq (x, y))],z)
| Gt (e1, e2) -> let r1 = translateB e1 in
                 let r2 = translateB e2 in
                 let x = freshName() in
                 let y = freshName() in
                 let z = freshName() in
                 ((fst r1)
                 @ [IRC_Assign (x, IRC_Var (snd r1))]
                 @ (fst r2)
                 @ [IRC_Assign (x, IRC_Var (snd r2));
                    IRC_Assign (z, IRC_Gt (x, y))], z)
| Not (e1) -> let r1 = translateB e1 in
              let x = freshName() in 
              ((fst r1)
              @ [IRC_Assign (x, IRC_Not(snd r1))], x)
| Plus (e1, e2) -> let r1 = translateB e1 in
                   let r2 = translateB e2 in
                   let x = freshName() in
                   let y = freshName() in
                   let z = freshName () in
                   ((fst r1)
                    @ [IRC_Assign (x, IRC_Var (snd r1))]
                    @ (fst r2)
                    @ [IRC_Assign (y, IRC_Var (snd r2))]
                    @ [IRC_Assign (z, IRC_Plus (x, y))] ,z)
| Minus (e1, e2) -> let r1 = translateB e1 in
                    let r2 = translateB e2 in
                    let x = freshName() in
                    let y = freshName() in
                    let z = freshName () in
                    ((fst r1)
                    @ [IRC_Assign (x, IRC_Var (snd r1))]
                    @ (fst r2)
                    @ [IRC_Assign (y, IRC_Var (snd r2))]
                    @ [IRC_Assign (z, IRC_Minus (x, y))] ,z)
| Times (e1, e2) -> let r1 = translateB e1 in
                    let r2 = translateB e2 in
                    let x = freshName() in
                    let y = freshName() in
                    let z = freshName () in
                    ((fst r1)
                    @ [IRC_Assign (x, IRC_Var (snd r1))]
                    @ (fst r2)
                    @ [IRC_Assign (y, IRC_Var (snd r2))]
                    @ [IRC_Assign (z, IRC_Times (x, y))],z)
| Division (e1, e2) -> let r1 = translateB e1 in
                       let r2 = translateB e2 in
                       let x = freshName() in
                       let y = freshName() in
                       let z = freshName () in
                       ((fst r1)
                       @ [IRC_Assign (x, IRC_Var (snd r1))]
                       @ (fst r2)
                       @ [IRC_Assign (y, IRC_Var (snd r2))]
                       @ [IRC_Assign (z, IRC_Division (x, y))],z)
| IConst i -> let x = freshName() in
              ([IRC_Assign (x, IRC_IConst (i))], x)
| Var (x) -> let y = lookup x in
             ([IRC_Assign (y, IRC_Var(y))], y) (* !!!NOT SURE *)
| RcvExp (x) -> let y = freshName() in ([IRC_Skip], y)
| FuncExp (s, es) -> let y = freshName() in
                     let l1 = freshLabel() in
                     let expls = List.map translateB es in
                     let res = List.fold_left (fun acc el -> acc@(fst el)) [] expls in
                     let eval = List.fold_left (fun acc el -> acc@[snd el]) [] expls in
                     let initParams = List.map (fun el -> IRC_Param el) eval in
                     let initLocals = List.map (fun el -> IRC_Param (lookup el)) (Hashtbl.find funcLocals s) in
                     (res
                      @ initLocals
                      @ initParams
                      @ [IRC_Return l1] (* Save the return address *)
                      @ [IRC_Call (s, List.length initParams)]
                      @ [IRC_Assign (y, IRC_Get)], y)

(* We do not return a tuple of (IRC_cmd list, value) because there is nothing to evaluate *)
let rec translateStmt s : (irc_cmd list) = match s with
    | Seq (s1, s2) -> 
      let left_stmt = translateStmt s1 in
      let right_stmt = translateStmt s2 in
      left_stmt @ right_stmt
    | Go (s1) -> translateStmt s1
    | Decl (t, s1, e1) -> let r1 = translateB e1 in
                          let x = freshName() in
                          Hashtbl.add tbl s1 x;
                          (fst r1) @ [IRC_Assign (x, IRC_Var (snd r1))]
    | Assign (s1, e1) -> let r1 = translateB e1 in
                         let y = lookup s1 in
                         (fst r1) @ [IRC_Assign (y, IRC_Var (snd r1))]
    | While (e1, (locals, s1)) -> 
                                  let r1 = translateB e1 in
                                  let l1 = freshLabel() in
                                  let l2 = freshLabel() in
                                  [IRC_Label l1]
                                  @ (fst r1)
                                  @ irc_ZeroJump ((snd r1), l2) 
                                  @ (translateStmt s1)
                                  @ [IRC_Goto l1]
                                  @ [IRC_Label l2]
    | ITE (e1, (locals_s1, s1), (locals_s2, s2)) -> let r1 = translateB e1 in
                                                    let r2 = translateStmt s1 in
                                                    let r3 = translateStmt s2 in
                                                    let l1 = freshLabel() in
                                                    let l2 = freshLabel() in
                                                    (fst r1)
                                                    @ irc_ZeroJump ((snd r1), l2)
                                                    @ r2
                                                    @ [IRC_NonzeroJump ((snd r1), l1)]
                                                    @ [IRC_Label l2]
                                                    @ r3
                                                    @ [IRC_Label l1]
    | Return e1 -> let r1 = translateB e1 in
                   let x = freshName() in
                   (fst r1)
                   @ [IRC_Assign (x, IRC_Var(snd r1))]
                   @ [IRC_Return x]
    | FuncCall (s1, es) -> let expls = List.map translateB es in
                           let res = List.fold_left (fun acc el -> acc@(fst el)) [] expls in
                           let eval = List.fold_left (fun acc el -> acc@[snd el]) [] expls in
                           let initParams = List.map (fun el -> IRC_Param el) eval in
                           let initLocals = List.map (fun el -> IRC_Param (lookup el)) (Hashtbl.find funcLocals s1) in
                           let l1 = freshLabel() in
                           res
                           @ initLocals
                           @ initParams
                           @ [IRC_Return l1] (* Save the return address *)
                           @ [IRC_Call (s1, List.length initParams)]
    | Print (e1) -> let r1 = translateB e1 in
                    let x = freshName() in
                    (fst r1) @ [IRC_Assign (x, IRC_Var(snd r1))] @ [IRC_Skip]
    | Skip -> [IRC_Skip]
    | DeclChan (x) -> [IRC_Skip]
    | RcvStmt (x) -> [IRC_Skip]
    | Transmit (x, e1) -> [IRC_Skip]

let removeTypes locals = match locals with 
  | Locals(ls) -> List.map (fun el -> fst el) ls

let translateProc func : (irc_cmd list) = match func with
  | Proc (name, args, ty, (locals, stmt)) -> let lcls = removeTypes locals in
                                                let l1 = freshLabel() in
                                                Hashtbl.add funcLocals name lcls;
                                                [IRC_Label l1]
                                                @ [IRC_Proc (lcls, name, l1)]
                                                @ translateStmt stmt

let translateProcs procs : (irc_cmd list) =
  List.flatten (List.map translateProc procs)

let rec translateProg p : irc option = match p with
  | Prog (procs, stmt) -> Some (IRC ((translateProcs procs) @ (translateStmt stmt)))

let remove_skips irc = match irc with
  | IRC cmds -> IRC (List.filter (fun s -> s <> IRC_Skip) cmds)
