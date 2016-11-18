open Intermediate
open Vm

let tbl = Hashtbl.create 100 (* Hashtable for remembering labels -> line_number mapping *)
let funcTbl = Hashtbl.create 100 (* Hashtable for remembering function names to labels mapping *)

(* Walk through the lists again to replace Goto labels with their specific instruction line number *)
let gotoLabel irc = List.map 
	(fun instr -> match instr with
		| Jump x -> let lineNo = try (Hashtbl.find tbl x) with
					| Not_found -> print_endline ("[Codegen] Cannot find label: " ^ (string_of_int x)); 1
					in Jump lineNo
		| NonZero x -> let lineNo = try (Hashtbl.find tbl x) with
							 | Not_found -> print_endline ("[Codegen] Cannot find label: " ^ (string_of_int x)); 1
							 in NonZero lineNo 
		| el -> el
	) irc

(* Takes a list of IRC commands and outputs a (final_instr_no., list of VM instructions) tuple,
   Replaces symbolic labels with current instruction line number
   i is starting instruction line number which is 0
 *)
let codegen i (irc : Intermediate.irc) = match irc with
  | IRC(cmds) -> let res = List.fold_left
				( fun ls el -> match el with
					| IRC_Assign (s1, IRC_IConst n) -> (fst ls)+1, (snd ls)@[Assign (s1, n)]
					| IRC_Assign (s1, IRC_Plus (x, y)) -> (fst ls)+5, (snd ls)@[PushToStack x;PushToStack y;Add;AssignFromStack (1, s1);PopS]
					| IRC_Assign (s1, IRC_Minus(x, y)) -> (fst ls)+5, (snd ls)@[PushToStack x;PushToStack y;Sub;AssignFromStack (1, s1);PopS]
					| IRC_Assign (s1, IRC_Times(x, y)) -> (fst ls)+5, (snd ls)@[PushToStack x;PushToStack y;Mult;AssignFromStack (1, s1);PopS]
					| IRC_Assign (s1, IRC_Division(x,y)) -> (fst ls)+5, (snd ls)@[PushToStack x;PushToStack y;Div;AssignFromStack (1, s1);PopS]
					| IRC_Assign (s1, IRC_Not (x)) -> (fst ls)+4, (snd ls)@[PushS 1;PushToStack x;Sub;AssignFromStack (1, s1);PopS]
					| IRC_Assign (s1, IRC_Gt (x, y)) -> (fst ls)+5, (snd ls)@[PushToStack x;PushToStack y;Gt;AssignFromStack (1, s1);PopS]
					| IRC_Assign (s1, IRC_Eq (x, y)) -> (fst ls)+5, (snd ls)@[PushToStack x;PushToStack y;Eq;AssignFromStack (1, s1);PopS]
					| IRC_Assign (s1, IRC_And (x, y)) -> (fst ls)+1, (snd ls)@[Halt] (* And will never appear in VM, shown here for complete matching sake *)
					| IRC_Assign (s1, IRC_Var (x)) -> (fst ls)+3, (snd ls)@[PushToStack x;AssignFromStack (1, s1);PopS]
					| IRC_Assign (s1, IRC_Get) -> (fst ls)+2, (snd ls)@[AssignFromEnv (1, s1);PopE]
					| IRC_Label (l1) -> Hashtbl.add tbl l1 ((fst ls)+1) ;(fst ls), (snd ls)@[] (* We do not generate any VM instruction, but update label -> instr mapping *)
					| IRC_Goto (l1) -> (fst ls)+1, (snd ls)@[Jump l1] (* we will replace them later once the hashtable is guaranteed to contain the mapping *)
					| IRC_NonzeroJump (s1, l1) -> (fst ls)+2, (snd ls)@[PushToStack s1;NonZero l1]
					| IRC_Param (s1) -> (fst ls)+1, (snd ls)@[PushToEnv s1]
					| IRC_Call (s1, nos) -> let l1 = Hashtbl.find funcTbl s1 in
											(fst ls)+1, (snd ls)@[Jump l1]
					| IRC_Return (x) -> (fst ls)+1, (snd ls)@[PushE ((fst ls)+2)] (* +2 because we want to skip the following Jump for FuncCall *)
(* 					| IRC_Get (s1) -> (fst ls)+2 (snd ls)@[AssignFromEnv (1, s1);PopE] *)
					| IRC_Proc (lcls, name, l1) -> Hashtbl.add funcTbl name l1;(fst ls)+10, (snd ls)@[]
					| IRC_Skip -> (fst ls)+1, (snd ls)@[Halt]
				) (i , []) cmds in
			  print_endline ("No. of VM instructions : " ^ string_of_int ((fst res)+1));
			  Some(gotoLabel (snd res))