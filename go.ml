type prog = Prog of (proc list) * stmt

and proc = Proc of string * ((exp * types) list) * (types option) * stmt

and types = TyInt
           | TyBool
           | TyChan of types
           | TyFunc of (types list * types)

and stmt = Seq of stmt * stmt
          | Go of stmt
          | Transmit of string * exp
          | RcvStmt of string
          | Decl of string * exp (* Done - update not yet implemented*)
          | DeclChan of string
          | Assign of string * exp (* Done *)
          | While of exp * stmt
          | ITE of exp * stmt * stmt
          | Return of exp
          | FuncCall of string * (exp list)
          | Print of exp
          | Skip

and exp = And of exp * exp (* Done *)
         | Eq of exp * exp (* Done *)
         | Gt of exp * exp (* Done *)
         | Plus of exp * exp (* Done *)
         | Minus of exp * exp (* Done *)
         | Times of exp * exp (* Done *)
         | Division of exp * exp (* Done *)
         | Not of exp (* Done *)
         | RcvExp of string (* Unsure *)
         | IConst of int (* Done *)
         | BConst of bool (* Done *)
         | Var of string (* Done *)
         | FuncExp of string * (exp list) (* Unsure *)

let rec pretty_print_prog s = match s with
  | Prog(xs, y) -> String.concat " " [">>>"; pretty_print_proc_list xs; "<<<"; pretty_print_stmt y]
and pretty_print_proc s = match s with
  | Proc(x, ys, z, s) -> String.concat " " ["func"; pretty_print_exp_type_list ys; print_type_option z; "{"; pretty_print_stmt s; "}"]
and pretty_print_proc_list s = match s with
  | [] -> ""
  | hd::tl -> String.concat "; " ["["; pretty_print_proc hd; pretty_print_proc_list tl; "]"]
and pretty_print_type s = match s with
  | TyInt   -> "Int"
  | TyBool  -> "Bool"
  | TyChan t -> String.concat " " ["Chan("; pretty_print_type t; ")"]
  | TyFunc (ts, t) -> String.concat " " ["Func("; pretty_print_type_list ts; ") -> "; pretty_print_type t]
and pretty_print_type_list s = match s with
  | [] -> ""
  | hd::tl -> String.concat "; " ["["; pretty_print_type hd; pretty_print_type_list tl; "]"]
and pretty_print_stmt s = match s with
  | Seq(x,y) -> String.concat " " [pretty_print_stmt x; pretty_print_stmt y]
  | Go(x)    -> String.concat " " ["{"; pretty_print_stmt x; "}"]
  | Transmit(x, y) -> String.concat " " [x; "<-"; pretty_print_exp y]
  | RcvStmt(x) -> x
  | Decl(x,y) -> String.concat " " [x; ":="; pretty_print_exp y]
  | DeclChan(x) -> x
  | Assign(x,y) -> String.concat " " [x; "="; pretty_print_exp y]
  | While(x, y) -> String.concat " " ["While"; pretty_print_exp x; "{"; pretty_print_stmt y; "}"]
  | ITE(x,y,z) -> String.concat " " ["if"; pretty_print_exp x; "{"; pretty_print_stmt y; "} else {"; pretty_print_stmt z; "}"]
  | Return(x) -> String.concat " " ["return"; pretty_print_exp x]
  | FuncCall(x, ys) -> String.concat " " [x; "("; pretty_print_exp_list ys; ")"]
  | Print(x) -> String.concat " " ["print"; pretty_print_exp x]
  | Skip -> "Skip"
and pretty_print_exp s = match s with
  | And(x,y) -> String.concat " " [pretty_print_exp x; "&&"; pretty_print_exp y]
  | Eq(x,y) -> String.concat " " [pretty_print_exp x; "=="; pretty_print_exp y]
  | Gt(x,y) -> String.concat " " [pretty_print_exp x; ">"; pretty_print_exp y]
  | Plus(x,y) -> String.concat " " [pretty_print_exp x; "+"; pretty_print_exp y]
  | Minus(x,y) -> String.concat " " [pretty_print_exp x; "-"; pretty_print_exp y]
  | Times(x,y) -> String.concat " " [pretty_print_exp x; "*"; pretty_print_exp y]
  | Division(x,y) -> String.concat " " [pretty_print_exp x; "/"; pretty_print_exp y]
  | Not(x) -> String.concat " " ["!"; pretty_print_exp x]
  | RcvExp(x) -> x
  | IConst(x) -> string_of_int(x)
  | BConst(x) -> string_of_bool(x)
  | Var(x) -> x
  | FuncExp(x, ys) -> String.concat " " [x; pretty_print_exp_list ys]
and pretty_print_exp_list s = match s with
  | [] -> ""
  | hd::tl -> String.concat "; " ["["; pretty_print_exp hd; pretty_print_exp_list tl; "]"]
and print_type_option r = match r with
  | Some(x)-> "Some(" ^ pretty_print_type x ^ ")"
  | None -> "None"
and pretty_print_exp_type_list r = match r with
  | [] -> ""
  | (e, t)::tl -> String.concat "; " ["["; "("; pretty_print_exp e; ", "; pretty_print_type t; ")"; pretty_print_exp_type_list tl; "]"]
