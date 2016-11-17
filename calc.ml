(* File calc.ml *)
open Go
(* open Intermediate *)
(* open Ty *)
(* open Vm *)
open Normalize
open Vm
open Codegen

(* Here is a rough overview of the compiler stages *)
(* Source --Parser--> AST --TypeCheck--> AST --Intermediate--> IR --CodeGen--> VM Code *)

let ( |> ) x f = f x (* pipeline function *)

(* PARSER STAGES *)
let parser src =
  let lexbuf = Lexing.from_channel (open_in src) in
  try
  	match (Parser.prog Lexer.token lexbuf) with
  	| Go.Prog(procls, stmt) -> Some (Go.Prog(procls, stmt))
  with Lexer.Eof ->
  	None

let typeCheck (ast : Go.prog) =
  let typeCheckProg x y =
    if (List.length x) > 0
    then Some y
  	else None
  in typeCheckProg [] ast

let intermediate (ast : Go.prog) = Intermediate.translateProg ast

let codeGen (irc : Intermediate.irc) = Codegen.codegen 0 irc

(* PRINTING FUNCTIONS *)

(* Print filename followed by AST *)
let printAst src ast = 
  Printf.printf "%s" (src); print_newline();
  Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout;
  ast

(* Chain all the stages together *)

let compiler src = src 
  |> parser
  |> (fun s -> match s with
      | None -> raise (Failure "Parsing error!")
      | Some p -> p)
  (* |> typeCheck
  |> (fun s -> match s with
      | None -> raise (Failure "Type error!")
      | Some p -> p) *)
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
  (* |> (fun s -> (Printf.printf "\nIntermediate Code:\n%s" (Intermediate.show_irc s)); print_newline(); flush stdout; s) *)
  |> codeGen
  |> (fun s -> match s with
      | None -> raise (Failure "VM code generation error!")
      | Some p -> p)
  (* |> (fun s -> (Printf.printf "\nVM Code:\n%s" (String.concat "\n" (List.map Vm.show_instructions s))); print_newline(); flush stdout; s) *)
  |> Vm.run

(* Testing *)
let parserTests = [
	(* "./tests/ex1.go"; *)
	(* "./tests/ex2.go"; *)
	(* "./tests/ex3.go" *)
  "./tests/normalizeTest2.go"
]

let typeCheckerTests = [
  (* "./tests/typeCheckerDeclarationsTest.go" *)
  (* "./tests/typeCheckerDeclarationsSpecialTest.go" *)
  (* "./tests/typeCheckerFunctionTest.go" *)
  "./tests/typeCheckerFunctionFailureTest.go"
  (* "./tests/typeCheckerSampleProgram.go" *)
]

(* Returns filename and AST tuple, given filename *)
let parseAst src =
  match (parser src) with
  | Some ast -> (src, ast)
  | None -> (src, Go.Prog ([], Skip))

let parseNormAst src =
  match (parser src) with
  | Some ast -> (src, Normalize.renameProg (Normalize.normalizeProg ast))
  | None -> (src, Go.Prog ([], Skip))


(* Loops through all files and prints out AST *)
(* 
let testParser =
  let parserAstList = List.map parseAst parserTests in
  	List.map printAst parserAstList

let testNormParser = 
  let parserNormAstList = List.map parseNormAst parserTests in
    List.map printNormAst parserNormAstList

let testTypeChecker =
  let parserAstList = List.map parseAst typeCheckerTests in
    List.map testAst parserAstList
 *)

(* let _ = testParser *)


let _ = compiler "./tests/normalizeTest2.go" 
