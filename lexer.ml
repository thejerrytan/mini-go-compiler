# 2 "lexer.mll"
 
open Parser        (* The type token is defined in parser.mli *)
open String
open Lexing

exception Eof
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

# 18 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\219\255\220\255\221\255\222\255\223\255\224\255\225\255\
    \227\255\228\255\229\255\230\255\231\255\232\255\233\255\234\255\
    \235\255\236\255\237\255\238\255\239\255\240\255\022\000\016\000\
    \027\000\015\000\026\000\030\000\024\000\031\000\023\000\038\000\
    \022\000\254\255\001\000\003\000\020\000\037\000\253\255\029\000\
    \032\000\027\000\042\000\252\255\045\000\244\255\251\255\029\000\
    \250\255\243\255\031\000\046\000\249\255\032\000\032\000\036\000\
    \041\000\248\255\047\000\045\000\053\000\247\255\051\000\047\000\
    \042\000\246\255\040\000\000\000\056\000\064\000\052\000\053\000\
    \063\000\057\000\245\255\055\000\059\000\242\255\071\000\059\000\
    \002\000\065\000\061\000\056\000\241\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\255\255\001\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default = 
   "\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\000\000\255\255\
    \000\000\000\000\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\000\000\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\035\000\033\000\033\000\035\000\034\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \035\000\008\000\081\000\035\000\000\000\000\000\014\000\000\000\
    \006\000\005\000\016\000\019\000\007\000\018\000\000\000\017\000\
    \020\000\020\000\020\000\020\000\020\000\020\000\020\000\020\000\
    \020\000\020\000\012\000\011\000\009\000\015\000\010\000\000\000\
    \000\000\000\000\000\000\068\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\021\000\023\000\022\000\021\000\028\000\031\000\030\000\
    \021\000\029\000\021\000\021\000\021\000\021\000\024\000\021\000\
    \025\000\021\000\027\000\021\000\032\000\021\000\021\000\026\000\
    \021\000\021\000\021\000\004\000\013\000\003\000\078\000\075\000\
    \066\000\062\000\058\000\053\000\050\000\048\000\046\000\040\000\
    \036\000\037\000\038\000\044\000\041\000\047\000\042\000\043\000\
    \045\000\049\000\051\000\052\000\054\000\055\000\056\000\057\000\
    \059\000\060\000\061\000\039\000\063\000\064\000\065\000\067\000\
    \069\000\070\000\071\000\072\000\073\000\074\000\076\000\077\000\
    \079\000\080\000\082\000\083\000\084\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\034\000\035\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\080\000\035\000\255\255\255\255\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\255\255\255\255\067\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\022\000\023\000\
    \024\000\025\000\026\000\027\000\028\000\029\000\030\000\031\000\
    \032\000\036\000\037\000\039\000\040\000\029\000\041\000\042\000\
    \044\000\047\000\050\000\051\000\053\000\054\000\055\000\056\000\
    \058\000\059\000\060\000\031\000\062\000\063\000\064\000\066\000\
    \068\000\069\000\070\000\071\000\072\000\073\000\075\000\076\000\
    \078\000\079\000\081\000\082\000\083\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token lexbuf =
    __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 21 "lexer.mll"
                   ( token lexbuf )
# 163 "lexer.ml"

  | 1 ->
# 22 "lexer.mll"
                   ( token lexbuf )
# 168 "lexer.ml"

  | 2 ->
# 23 "lexer.mll"
                   ( TRUE )
# 173 "lexer.ml"

  | 3 ->
# 24 "lexer.mll"
                   ( FALSE )
# 178 "lexer.ml"

  | 4 ->
# 25 "lexer.mll"
                   ( GO )
# 183 "lexer.ml"

  | 5 ->
# 26 "lexer.mll"
                   ( IF )
# 188 "lexer.ml"

  | 6 ->
# 27 "lexer.mll"
                   ( ELSE )
# 193 "lexer.ml"

  | 7 ->
# 28 "lexer.mll"
                   ( RETURN )
# 198 "lexer.ml"

  | 8 ->
# 29 "lexer.mll"
                   ( WHILE )
# 203 "lexer.ml"

  | 9 ->
# 30 "lexer.mll"
                   ( PRINT )
# 208 "lexer.ml"

  | 10 ->
# 31 "lexer.mll"
                   ( NEWCHANNEL )
# 213 "lexer.ml"

  | 11 ->
# 32 "lexer.mll"
                   ( FUNC )
# 218 "lexer.ml"

  | 12 ->
# 33 "lexer.mll"
                   ( INT_TYPE )
# 223 "lexer.ml"

  | 13 ->
# 34 "lexer.mll"
                   ( BOOL_TYPE )
# 228 "lexer.ml"

  | 14 ->
# 35 "lexer.mll"
                   ( CHANNEL_TYPE )
# 233 "lexer.ml"

  | 15 ->
# 36 "lexer.mll"
                   ( LETTER ((Lexing.lexeme lexbuf).[0]) )
# 238 "lexer.ml"

  | 16 ->
# 37 "lexer.mll"
                   ( DIGIT ((Lexing.lexeme lexbuf).[0]) )
# 243 "lexer.ml"

  | 17 ->
# 38 "lexer.mll"
                   ( PLUS )
# 248 "lexer.ml"

  | 18 ->
# 39 "lexer.mll"
                   ( MINUS )
# 253 "lexer.ml"

  | 19 ->
# 40 "lexer.mll"
                   ( DIVIDE )
# 258 "lexer.ml"

  | 20 ->
# 41 "lexer.mll"
                   ( TIMES )
# 263 "lexer.ml"

  | 21 ->
# 42 "lexer.mll"
                   ( EQUAL )
# 268 "lexer.ml"

  | 22 ->
# 43 "lexer.mll"
                   ( AMP )
# 273 "lexer.ml"

  | 23 ->
# 44 "lexer.mll"
                   ( BAR )
# 278 "lexer.ml"

  | 24 ->
# 45 "lexer.mll"
                   ( COLON )
# 283 "lexer.ml"

  | 25 ->
# 46 "lexer.mll"
                   ( SEMICOLON )
# 288 "lexer.ml"

  | 26 ->
# 47 "lexer.mll"
                   ( RANGLE )
# 293 "lexer.ml"

  | 27 ->
# 48 "lexer.mll"
                   ( LANGLE )
# 298 "lexer.ml"

  | 28 ->
# 49 "lexer.mll"
                   ( EXCLAIM )
# 303 "lexer.ml"

  | 29 ->
# 50 "lexer.mll"
                   ( DASH )
# 308 "lexer.ml"

  | 30 ->
# 51 "lexer.mll"
                   ( COMMA )
# 313 "lexer.ml"

  | 31 ->
# 52 "lexer.mll"
                   ( LPAREN )
# 318 "lexer.ml"

  | 32 ->
# 53 "lexer.mll"
                   ( RPAREN )
# 323 "lexer.ml"

  | 33 ->
# 54 "lexer.mll"
                   ( LBRACE )
# 328 "lexer.ml"

  | 34 ->
# 55 "lexer.mll"
                   ( RBRACE )
# 333 "lexer.ml"

  | 35 ->
# 56 "lexer.mll"
                   ( raise (SyntaxError ("Unexpected char:" ^ Lexing.lexeme lexbuf)) )
# 338 "lexer.ml"

  | 36 ->
# 57 "lexer.mll"
                   ( raise Eof )
# 343 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

;;

