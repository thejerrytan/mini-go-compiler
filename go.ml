type prog = Prog of (proc list) * stmt (* Done *)

and proc = Proc of string * ((exp * types) list) * (types option) * (locals * stmt) (* To be implemented *)

and types = TyInt (* OK *)
           | TyBool (* OK *)
           | TyChan of types (* OK *)
           | TyFunc of (types list * (types option)) (* OK *)

and stmt = Seq of stmt * stmt (* Done *)
          | Go of stmt (* Done *)
          | Transmit of string * exp (* Done *)
          | RcvStmt of string (* Done *)
          | Decl of (types option) * string * exp (* Done *)
          | DeclChan of string (* Done *)
          | Assign of string * exp (* Done *)
          | While of exp * (locals * stmt) (* Done *)
          | ITE of exp * (locals * stmt) * (locals * stmt) (* Done *)
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
         (* | SkipExp *)
and locals = Locals of (string * types) list

let string_repeat s n =
  let len = Bytes.length s in
  let res = Bytes.create(n * len) in
  for i = 0 to pred n do
    Bytes.blit s 0 res (i * len) len
  done;
  Bytes.to_string res

let indent d = (if d > 0 then ((string_repeat "|   " (d - 1)) ^ "├── ") else "")

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
  | FuncExp(x, ys) -> linify [(indent d) ^ "FuncExp"; print_string (d + 1) x; print_exp_list (d + 1) ys]
  | Not(x) -> linify [(indent d) ^ "Not"; print_exp (d + 1) x]
  | RcvExp(x) -> linify [(indent d) ^ "RcvExp " ^ x]
  | IConst(x) -> linify [(indent d) ^ "IConst " ^ (string_of_int x)]
  | BConst(x) -> linify [(indent d) ^ "BConst " ^ (string_of_bool x)]
  | Var(x) -> linify [(indent d) ^ "Var " ^ x]
  (* | SkipExp -> linify [(indent d) ^ "SkipExp"] *)

and print_exp_list d s = 
  linify (((indent d) ^ "List") :: (List.map (print_exp (d + 1)) s))

let rec print_type d s = match s with
  | TyInt   -> (indent d) ^ "TyInt"
  | TyBool  -> (indent d) ^ "TyBool"
  | TyChan t -> linify [(indent d) ^ "TyChan"; print_type (d + 1) t;]
  | TyFunc (ts, t) -> match t with
    | Some t -> linify [(indent d) ^ "TyFunc"; print_type_list (d + 1) ts; print_type (d + 1) t]
    | None -> linify [(indent d) ^ "TyFunc"; print_type_list (d + 1) ts; (indent (d+1)) ^ "None"]
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
  | Decl(t,x,y) -> linify [(indent d) ^ "Decl"; print_type_option (d + 1) t; print_string (d + 1) x; print_exp (d + 1) y]
  | DeclChan(x) -> linify [(indent d) ^ "DeclChan"; print_string (d + 1) x]
  | Assign(x,y) -> linify [(indent d) ^ "Assign"; print_string (d + 1) x; print_exp (d + 1) y]
  | While(x, (locals, y)) -> linify [(indent d) ^ "While"; print_exp (d + 1) x; print_stmt (d + 1) y]
  | ITE(x,(localy, y), (localz, z)) -> linify [(indent d) ^ "ITE"; print_exp (d + 1) x; print_stmt (d + 1) y; print_stmt (d + 1) z]
  | Return(x) -> linify [(indent d) ^ "Return"; print_exp (d + 1) x]
  | FuncCall(x, ys) -> linify [(indent d) ^ "FuncCall"; print_string (d + 1) x; print_exp_list (d + 1) ys]
  | Print(x) -> linify [(indent d) ^ "Print"; print_exp (d + 1) x]
  | Skip -> (indent d) ^ "Skip"

let print_proc d s = match s with
  | Proc(x, ys, z, (locals,s)) -> linify [(indent d) ^ "Proc"; print_string (d + 1) x; print_exp_type_list (d + 1) ys; print_type_option (d + 1) z; print_stmt (d + 1) s]

let print_proc_list d s = 
  linify (((indent d) ^ "List") :: (List.map (print_proc (d + 1)) s))

let rec print_prog d s = match s with
  | Prog(xs, y) -> linify [(indent d) ^ "Prog"; print_proc_list (d + 1) xs; print_stmt (d + 1) y]
