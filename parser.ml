type token =
  | LETTER of (char)
  | PLUS
  | STAR
  | LPAREN
  | RPAREN
  | EOL

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"
     open Re 
  
    
# 16 "parser.ml"
let yytransl_const = [|
  258 (* PLUS *);
  259 (* STAR *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* LETTER *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\007\000\000\000\000\000\000\000\
\006\000\001\000\000\000\003\000\000\000"

let yydgoto = "\002\000\
\005\000\011\000"

let yysindex = "\001\000\
\000\255\000\000\000\000\000\255\000\000\007\255\013\255\000\255\
\000\000\000\000\018\255\000\000\018\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\255\000\000\021\255"

let yygindex = "\000\000\
\000\000\255\255"

let yytablesize = 27
let yytable = "\006\000\
\003\000\001\000\007\000\004\000\000\000\000\000\013\000\003\000\
\008\000\009\000\004\000\000\000\010\000\003\000\008\000\009\000\
\004\000\012\000\003\000\008\000\009\000\004\000\000\000\005\000\
\005\000\004\000\004\000"

let yycheck = "\001\000\
\001\001\001\000\004\000\004\001\255\255\255\255\008\000\001\001\
\002\001\003\001\004\001\255\255\006\001\001\001\002\001\003\001\
\004\001\005\001\001\001\002\001\003\001\004\001\255\255\005\001\
\006\001\005\001\006\001"

let yynames_const = "\
  PLUS\000\
  STAR\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  LETTER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 15 "parser.mly"
                                    ( _1 )
# 85 "parser.ml"
               : Re.re))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : char) in
    Obj.repr(
# 18 "parser.mly"
                                       ( Letter _1 )
# 92 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 19 "parser.mly"
                                    ( _2 )
# 99 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 20 "parser.mly"
                                    ( Alt (_1,_3) )
# 107 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 21 "parser.mly"
                                    ( Seq (_1,_2) )
# 115 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 22 "parser.mly"
                                    ( Star _1 )
# 122 "parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Re.re)
