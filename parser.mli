type token =
  | NAME of string
  | INT of int
  | TRUE of bool
  | FALSE of bool
  | GO
  | IF
  | ELSE
  | RETURN
  | WHILE
  | PRINT
  | NEWCHANNEL
  | INT_TYPE
  | BOOL_TYPE
  | CHANNEL_TYPE
  | PLUS
  | MINUS
  | DIVIDE
  | TIMES
  | EQUAL
  | AMP
  | BAR
  | COLON
  | SEMICOLON
  | RANGLE
  | LANGLE
  | EXCLAIM
  | DASH
  | COMMA
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | EOL

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Go.go
