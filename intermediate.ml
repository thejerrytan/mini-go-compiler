(* Intermediate.ml *)
open Go

let nameSupply = ref 0
let freshName _ =  nameSupply := !nameSupply + 1;
                   String.concat "" ["_irc" ; string_of_int (!nameSupply )] 


type irc = IRC of (irc_cmd list)
[@@deriving show]

and irc_cmd = IRC_Assign of string * irc_exp
            | IRC_Label of int
            | IRC_Goto of int
            | IRC_NonzeroJump of string * int  (* if x L = if x non-zero then jump to L *)
            | IRC_Param of string (* Push param onto stack *)
            | IRC_Call of int * int (* (label, number of parameters *)
            | IRC_Return of string (* Push return value onto stack? *)
            | IRC_Get of string (* Get from stack? *)
            | IRC_Skip
(* Need another one for getting params from stack *)

and irc_exp = IRC_And of string * string
            | IRC_Eq of string * string
            | IRC_Gt of string * string
            | IRC_Plus of string * string
            | IRC_Minus of string * string
            | IRC_Times of string * string
            | IRC_Division of string * string
            | IRC_Not of string
            | IRC_IConst of int
            | IRC_Var of string                                    

(* Need another one for IRC_proc *)
and irc_proc = IRC_PROC of locals * string * int (* procedure call of string at memLoc int *)

(* short-hand for 'zero' jump *)
let irc_ZeroJump (x,l) = let y = freshName() in
                         [IRC_Assign (y, IRC_Not x);
                          IRC_NonzeroJump (y,l)]

let labelSupply = ref 1
let freshLabel _ =  labelSupply := !labelSupply + 1;
                    !labelSupply

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
| Var (x) -> let y = freshName() in
             ([IRC_Assign (y, IRC_Var(x))], y) (* !!!NOT SURE *)
| RcvExp (x) -> ([IRC_Skip], x)
| FuncExp (s, es) -> ([IRC_Skip], s)

(* let lookup id = 1 *)

(* We do not return a tuple of (IRC_cmd list, value) because there is nothing to evaluate *)
let rec translateStmt s : (irc_cmd list) = match s with
    | Seq (s1, s2) -> 
      let left_stmt = translateStmt s1 in
      let right_stmt = translateStmt s2 in
      left_stmt @ right_stmt
    | Go (s1) -> translateStmt s1
    | Decl (t, s1, e1) -> let r1 = translateB e1 in
                          let x = freshName() in
                          (fst r1) @ [IRC_Assign (x, IRC_Var (snd r1))]
    | Assign (s1, e1) -> let r1 = translateB e1 in
                         let x = freshName() in
                         (fst r1) @ [IRC_Assign (x, IRC_Var (snd r1))]
    | While (e1, (locals, s1)) -> 
                                  let r1 = translateB e1 in
                                  let l1 = freshLabel() in
                                  [IRC_Label l1]
                                  @ (translateStmt s1)
                                  @ (fst r1)
                                  @ [IRC_NonzeroJump ((snd r1), l1)]
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
    | FuncCall (s1, es) -> [IRC_Skip]
    | Print (e1) -> let r1 = translateB e1 in
                    let x = freshName() in
                    (fst r1) @ [IRC_Assign (x, IRC_Var(snd r1))] @ [IRC_Skip]
    | Skip -> [IRC_Skip]
    | DeclChan (x) -> [IRC_Skip]
    | RcvStmt (x) -> [IRC_Skip]
    | Transmit (x, e1) -> [IRC_Skip]

let translateProc proc : (irc_cmd list) = [] (* match proc with
  | Proc (name, arg_name_types, return_type_option, (locals * stmt)) ->
    [freshLabel ()] @
    (List.map () (List.length arg_name_types)) (* pop n times from stack *)
*)

let translateProcs procs : (irc_cmd list) =
  List.flatten (List.map translateProc procs)

let rec translateProg p : irc option = match p with
  | Prog (procs, stmt) -> Some (IRC ((translateProcs procs) @ (translateStmt stmt)))

