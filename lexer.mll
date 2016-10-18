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
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

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
  | ['a'-'z']      { LETTER ((Lexing.lexeme lexbuf).[0]) }
  | ['0'-'9']      { DIGIT ((Lexing.lexeme lexbuf).[0]) }
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