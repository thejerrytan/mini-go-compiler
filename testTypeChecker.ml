(* File testTypeChecker.ml *)
open Go
open Ty

let parser src =
  let lexbuf = Lexing.from_channel (open_in src) in
  try
    match (Parser.prog Lexer.token lexbuf) with
    | Go.Prog(procls, stmt) -> Some (Go.Prog(procls, stmt))
  with Lexer.Eof ->
    None

let typeCheckerTests = [
  "./tests/typeCheckerTests/typeCheckerDeclarationsTest.go";
  "./tests/typeCheckerTests/typeCheckerFunctionTest.go";
  "./tests/typeCheckerTests/typeCheckerSampleProgram.go";
  "./tests/typeCheckerTests/typeCheckerRecursiveFunction.go";
  "./tests/typeCheckerTests/typeCheckerMutuallyRecursiveFunction.go"
]

let parseAst src =
  match (parser src) with
  | Some ast -> (src, ast)
  | None -> (src, Go.Prog ([], Skip))

let printAst (src, ast) =
  Printf.printf "%s" (src); print_newline(); flush stdout;
  Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout

let testAst (src, ast) = match typeCheckProg [] ast with
                         | Some ast -> Printf.printf "%s" (src); print_newline(); flush stdout; Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout;
                         | None -> Printf.printf "%s" "fail"; print_newline(); flush stdout

let testTypeChecker =
  let parserAstList = List.map parseAst typeCheckerTests in
    List.map testAst parserAstList

let _ = testTypeChecker

(* Correctly fail with no return type but have return statement *)

(* Correctly fail with no return type but have return statements in ITE and While *)

(* Correctly fail with return type but have no return statement *)

(* Correctly pass with no return type and no return statement *)

(* Correctly pass with return type and have return statement *)

(* Correctly fail with return type int and declare a with int and then return an expression of type int *)

(* Correctly fail with return type and return type of wrong expression in ITE and While *)

(* Correctly fail with parameters type and not being given right number of types *)

(* Correctly fail with paramters type and not being given the right kind of types *)
