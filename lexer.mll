(* File lexer.mll *)
{
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
}
let int     = ['0'-'9'] ['0'-'9']*
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name    = ['a'-'z'] ['a'-'z']*
let id      = ['a'-'z'] ['a'-'z' '0'-'9']*

rule token = parse
    white          { token lexbuf }     (* skip blanks *)
  | newline        { token lexbuf }
  | "true"         { TRUE }
  | "false"        { FALSE }
  | "go"           { GO }
  | "if"           { IF }
  | "else"         { ELSE }
  | "return"       { RETURN }
  | "while"        { WHILE }
  | "print"        { PRINT }
  | "newChannel"   { NEWCHANNEL }
  | "func"         { FUNC }
  | "int"          { INT_TYPE }
  | "bool"         { BOOL_TYPE }
  | "chan int"     { CHANNEL_TYPE }
  | name           { print_string "test";NAME (Lexing.lexeme lexbuf) }
  | id             { print_string "test";VARS (Lexing.lexeme lexbuf) }
  | int            { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '/'            { DIVIDE }
  | '*'            { TIMES }
  | '='            { EQUAL }
  | '&'            { AMP }
  | '|'            { BAR }
  | ':'            { COLON }
  | ';'            { SEMICOLON }
  | '>'            { RANGLE }
  | '<'            { LANGLE }
  | '!'            { EXCLAIM }
  | '-'            { DASH }
  | ','            { COMMA }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | '{'            { LBRACE }
  | '}'            { RBRACE }
  | _              { raise (SyntaxError ("Unexpected char:" ^ Lexing.lexeme lexbuf)) }
  | eof            { raise Eof }