(* File calc.ml *)
open Go
(* open Intermediate *)
(* open Ty *)
(* open Vm *)
(* open Normalize *)

(* Here is a rough overview of the compiler stages *)
(* Source --Parser--> AST --TypeCheck--> AST --Intermediate--> IR --CodeGen--> VM Code *)

let parser src =
  let lexbuf = Lexing.from_channel (open_in src) in
  try
  	match (Parser.prog Lexer.token lexbuf) with
  	| Go.Prog(procls, stmt) -> Some (Go.Prog(procls, stmt))
  with Lexer.Eof ->
  	None


(* let typeCheck (ast : Go.prog) =
  let typeCheckProg x y =
    if (List.length x) > 0
    then Some y
  	else None
  in match typeCheckProg [] ast with
  | Some (x) -> Some x
  | None -> None

let intermediate (ast : Go.prog) = Some (Intermediate.IRC [])
let codeGen (ir : Intermediate.irc) = Some [Vm.Halt]

(* Chain all the stages together *)
let compiler src = match (parser src) with
  | None -> None
  | Some p -> match (typeCheck p) with
              | None -> None
              | Some p -> match (intermediate p) with
                          | None -> None
                          | Some i -> codeGen i
 *)
(* Testing *)
let parserTests = [
	"./tests/ex1.go";
	"./tests/ex2.go";
	"./tests/ex3.go"
  (* "./tests/normalizeTest.go" *)
  (* "./tests/typeCheckerDeclarationsTest.go" *)
  (* "./tests/typeCheckerFunctionTest.go" *)
  (* "./tests/typeCheckerFunctionFailureTest.go" *)
]

let typeCheckerTests = [
  "./tests/typeCheckerDeclarationsTest.go"
  (* "./tests/typeCheckerDeclarationsSpecialTest.go" *)
  (* "./tests/typeCheckerFunctionTest.go" *)
  (* "./tests/typeCheckerFunctionFailureTest.go" *)
]

(* Returns filename and AST tuple, given filename *)
let parseAst src =
  match (parser src) with
  | Some ast -> (src, ast)
  | None -> (src, Go.Prog ([], Skip))

(* Print filename followed by AST *)
let printAst (src, ast) =
	Printf.printf "%s" (src); print_newline(); flush stdout;
	Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout

(* let parseNormAst src =
  match (parser src) with
  | Some ast -> (src, Normalize.normalizeProg ast)
  | None -> (src, Go.Prog ([], Skip))

let printNormAst (src, ast) =
  Printf.printf "%s" (src); print_newline(); flush stdout;
  Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout
 *)

(* Loops through all files and prints out AST *)

let testParser =
  let parserAstList = List.map parseAst parserTests in
  	List.map printAst parserAstList

(* let testAst (src, ast) = match typeCheckProg [] ast with
                         | Some ast -> Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout;
                         | None -> Printf.printf "%s" "fail"; print_newline(); flush stdout
 *)
(* let testTypeChecker =
  let parserAstList = List.map parseAst typeCheckerTests in
    List.map testAst parserAstList
 *)
(* let testNormParser =
  let parserNormAstList = List.map parseNormAst parserTests in
    List.map printNormAst parserNormAstList *)

(* let _ = testTypeChecker *)
let _ = testParser
(* let _ = testNormParser  *)
