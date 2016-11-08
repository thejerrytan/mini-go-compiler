(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
open String
open Lexing

exception Eof
exception SyntaxError of string
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let letter = ['a'-'z']
let digit = ['0'-'9']
let name = letter+
let vars = letter | letter(letter|digit)+
let ints = ['0'-'9']+

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
  | ints as lxm    { INTS (lxm) }
  | name as lxm    { NAME (lxm) }
  | vars as lxm    { VARS (lxm) }
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