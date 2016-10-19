type prog = Prog of (proc list) * stmt (* Done *)

and proc = Proc of string * ((exp * types) list) * (types option) * stmt (* To be implemented *)

and types = TyInt (* OK *)
           | TyBool (* OK *)
           | TyChan of types (* OK *)
           | TyFunc of (types list * types) (* OK *)

and stmt = Seq of stmt * stmt (* Done *)
          | Go of stmt (* Done *)
          | Transmit of string * exp (* Done *)
          | RcvStmt of string (* Done *)
          | Decl of string * exp (* Done *)
          | DeclChan of string (* Done *)
          | Assign of string * exp (* Done *)
          | While of exp * stmt (* Done *)
          | ITE of exp * stmt * stmt (* Done *)
          | Return of exp (* Done *)
          | FuncCall of string * (exp list) (* Done *)
          | Print of exp (* Done *)
          | Skip (* Done *)

and exp = And of exp * exp (* Done *)
         | Eq of exp * exp (* Done *)
         | Gt of exp * exp (* Done *)
         | Plus of exp * exp (* Done *)
         | Minus of exp * exp (* Done *)
         | Times of exp * exp (* Done *)
         | Division of exp * exp (* Done *)
         | Not of exp (* Done *)
         | RcvExp of string (* Done *)
         | IConst of int (* Done *)
         | BConst of bool (* Done *)
         | Var of string (* Done *)
         | FuncExp of string * (exp list) (* Done *)

let indent d = (if d <= 1 then "" else (String.make (3 * (d - 1)) ' ')) ^ (if d > 0 then " | " else "")

let print_string d s = (indent d) ^ s

let linify xs = String.concat "\n" xs

let rec print_exp d s = match s with
  | And(x,y) -> linify [(indent d) ^ "And"; print_exp (d + 1) x; print_exp (d + 1) y]
  | Eq(x,y) -> linify [(indent d) ^ "Eq"; print_exp (d + 1) x; print_exp (d + 1) y]
  | Gt(x,y) -> linify [(indent d) ^ "Gt"; print_exp (d + 1) x; print_exp (d + 1) y]
  | Plus(x,y) -> linify [(indent d) ^ "Plus"; print_exp (d + 1) x; print_exp (d + 1) y]
  | Minus(x,y) -> linify [(indent d) ^ "Minus"; print_exp (d + 1) x; print_exp (d + 1) y]
  | Times(x,y) -> linify [(indent d) ^ "Times"; print_exp (d + 1) x; print_exp (d + 1) y]
  | Division(x,y) -> linify [(indent d) ^ "Division"; print_exp (d + 1) x; print_exp (d + 1) y]
  | Not(x) -> linify [(indent d) ^ "Not"; print_exp (d + 1) x]
  | RcvExp(x) -> linify [(indent d) ^ "RcvExp"; print_string (d + 1) x]
  | IConst(x) -> linify [(indent d) ^ "IConst"; print_string (d + 1) (string_of_int x)]
  | BConst(x) -> linify [(indent d) ^ "BConst"; print_string (d + 1) (string_of_bool x)]
  | Var(x) -> linify [(indent d) ^ "Var"; print_string (d + 1) x]
  | FuncExp(x, ys) -> linify [(indent d) ^ "FuncExp"; print_string (d + 1) x; print_exp_list (d + 1) ys]
and print_exp_list d s = 
  linify (((indent d) ^ "List") :: (List.map (print_exp (d + 1)) s))

let rec print_type d s = match s with
  | TyInt   -> (indent d) ^ "TyInt"
  | TyBool  -> (indent d) ^ "TyBool"
  | TyChan t -> linify [(indent d) ^ "TyChan"; print_type (d + 1) t;]
  | TyFunc (ts, t) -> linify [(indent d) ^ "TyFunc"; print_type_list (d + 1) ts; print_type (d + 1) t]
and print_type_list d s = 
  linify (((indent d) ^ "List") :: (List.map (print_type (d + 1)) s))

let print_type_option d s = match s with
  | Some(x)-> linify [(indent d) ^ "Some"; print_type (d + 1) x]
  | None -> (indent d) ^ "None"

let print_exp_type d s =
  linify [(indent d) ^ "Tuple"; print_exp (d + 1) (fst s); print_type (d + 1) (snd s)]

let print_exp_type_list d s =
  linify (((indent d) ^ "List") :: (List.map (print_exp_type (d + 1)) s))

let rec print_stmt d s = match s with
  | Seq(x,y) -> linify [(indent d) ^ "Seq"; print_stmt (d + 1) x; print_stmt (d + 1) y]
  | Go(x)    -> linify [(indent d) ^ "Go"; print_stmt (d + 1) x]
  | Transmit(x, y) -> linify [(indent d) ^ "Transmit"; print_string (d + 1) x; print_exp (d + 1) y]
  | RcvStmt(x) -> linify [(indent d) ^ "RcvStmt"; print_string (d + 1) x]
  | Decl(x,y) -> linify [(indent d) ^ "Decl"; print_string (d + 1) x; print_exp (d + 1) y]
  | DeclChan(x) -> linify [(indent d) ^ "DeclChan"; print_string (d + 1) x]
  | Assign(x,y) -> linify [(indent d) ^ "Assign"; print_string (d + 1) x; print_exp (d + 1) y]
  | While(x, y) -> linify [(indent d) ^ "While"; print_exp (d + 1) x; print_stmt (d + 1) y]
  | ITE(x,y,z) -> linify ["ITE"; print_exp (d + 1) x; print_stmt (d + 1) y; print_stmt (d + 1) z]
  | Return(x) -> linify ["Return"; print_exp (d + 1) x]
  | FuncCall(x, ys) -> linify [(indent d) ^ "FuncCall"; print_string (d + 1) x; print_exp_list (d + 1) ys]
  | Print(x) -> linify [(indent d) ^ "Print"; print_exp (d + 1) x]
  | Skip -> (indent d) ^ "Skip"

let print_proc d s = match s with
  | Proc(x, ys, z, s) -> linify [(indent d) ^ "Proc"; print_string (d + 1) x; print_exp_type_list (d + 1) ys; print_type_option (d + 1) z; print_stmt (d + 1) s]

let print_proc_list d s = 
  linify (((indent d) ^ "List") :: (List.map (print_proc (d + 1)) s))

let rec print_prog d s = match s with
  | Prog(xs, y) -> linify [(indent d) ^ "Prog"; print_proc_list (d + 1) xs; print_stmt (d + 1) y]
