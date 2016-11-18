(* File calc.ml *)
open Go
open Intermediate
open Ty
open Normalize
open Vm
open Codegen

(* Here is a rough overview of the compiler stages *)
(* Source --Parser--> AST --TypeCheck--> AST --Intermediate--> IR --CodeGen--> VM Code *)

let ( |> ) x f = f x (* pipeline function *)

(* PRINTING FUNCTIONS *)

(* Print filename followed by AST *)
let printAst src ast = 
  Printf.printf "%s" (src); print_newline();
  Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout;
  ast

(* PARSER STAGES *)
let parser src =
  let lexbuf = Lexing.from_channel (open_in src) in
  try
  	match (Parser.prog Lexer.token lexbuf) with
  	| Go.Prog(procls, stmt) -> Some (Go.Prog(procls, stmt))
  with Lexer.Eof ->
  	None

let typeCheck (ast : Go.prog) =
  match typeCheckProg [] ast with
    | Some x -> Some x
    | None -> None

let intermediate (ast : Go.prog) = Intermediate.translateProg ast

let codeGen (irc : Intermediate.irc) = Codegen.codegen 0 irc

let compiler src = src 
  |> parser
  |> (fun s -> match s with
      | None -> raise (Failure "Parsing error!")
      | Some p -> p)
  |> typeCheck
  |> (fun s -> match s with
      | None -> raise (Failure "Type error!")
      | Some p -> p)
  |> (fun s -> Printf.printf("\nBefore Normalize"); print_newline(); flush stdout; s)
  |> (printAst src)
  |> Normalize.normalizeProg
  |> Normalize.renameProg
  |> (fun s -> Printf.printf("\nAfter Normalize"); print_newline(); flush stdout; s)
  |> (printAst src)
  |> intermediate
  |> (fun s -> match s with
      | None -> raise (Failure "Intermediate code generation error!")
      | Some p -> p)
(*   |> Intermediate.remove_skips *)
  |> (fun s -> (Printf.printf "\nIntermediate Code:\n%s" (Intermediate.show_irc s)); print_newline(); flush stdout; s)
  |> codeGen
  |> (fun s -> match s with
      | None -> raise (Failure "VM code generation error!")
      | Some p -> p)
  |> (fun s -> (Printf.printf "\nVM Code:\n%s" (String.concat "\n" (List.map Vm.show_instructions s))); print_newline(); flush stdout; s)
  |> Vm.run

let _ = 
  if (Array.length Sys.argv < 2) then
    raise (Invalid_argument "Usage: ./calc.native (filename)")
  else
    let src = Sys.argv.(1) in
    compiler src