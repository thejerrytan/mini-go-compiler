type token =
  | LETTER of (char)
  | PLUS
  | STAR
  | LPAREN
  | RPAREN
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Re.re
