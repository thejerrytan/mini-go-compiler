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
  ("./tests/typeCheckerTests/typeCheckerSimpleValidTests.go",
   "Valid simple main body program");
  ("./tests/typeCheckerTests/typeCheckerSampleProgram.go",
   "Valid sample code from project website");
  ("./tests/typeCheckerTests/typeCheckerRecursiveFac.go",
   "Valid rewritten recursive fac from project website");
  ("./tests/typeCheckerTests/typeCheckerMutuallyRecursivePingPong.go",
   "Valid mutually recursive function written by us");
  ("./tests/typeCheckerTests/typeCheckerFunctionTest.go",
   "Valid declaration of multiple functions of various types");
  ("./tests/typeCheckerTests/typeCheckerFunctionNoReturnWithReturnStatement.go",
   "invalid declaration of function with no return type but with return statement");
  ("./tests/typeCheckerTests/typeCheckerFunctionNoReturnWithReturnStatementInIte.go",
   "invalid declaration of function with no return type but with return statement in ITE");
  ("./tests/typeCheckerTests/typeCheckerFunctionNoReturnWithReturnStatementInWhile.go",
   "invalid declaration of function with no return type but with return statement in While");
  ("./tests/typeCheckerTests/typeCheckerFunctionReturnWithWrongType.go",
   "invalid declaration of function with return type but has a return with another type");
  ("./tests/typeCheckerTests/typeCheckerFunctionReturnWithWrongTypeInIte.go",
   "invalid declaration of function with return type but has a return with another type in ITE");
  ("./tests/typeCheckerTests/typeCheckerFunctionReturnWithWrongTypeInWhile.go",
   "invalid declaration of function with return type but has a return with another type in While");
  ("./tests/typeCheckerTests/typeCheckerFunctionWithLessNumberOfParamsSupplied.go",
   "invalid declaration of function with parameters type but is called with too little parameters");
  ("./tests/typeCheckerTests/typeCheckerFunctionWithMoreNumberOfParamsSupplied.go",
   "invalid declaration of function with parameters type but is called with too many parameters");
  ("./tests/typeCheckerTests/typeCheckerFunctionWithWrongTypeOfParamsSupplied.go",
   "invalid declaration of function with parameters type but is called with wrong parameters type");
  ("./tests/typeCheckerTests/typeCheckerUndeclaredVariable.go",
   "invalid usage of undeclared variable");
  ("./tests/typeCheckerTests/typeCheckerIncompatibleTypes.go",
   "invalid incompatible types");
]

let parseAst srcr =
  let src = fst srcr in
    match (parser src) with
    | Some ast -> (srcr, ast)
    | None -> (srcr, Go.Prog ([], Skip)) (* Should never happen in this context *)

let testAst (srcr, ast) = match typeCheckProg [] ast with
                          | Some ast -> Printf.printf "%s - %s" (fst srcr) (snd srcr); print_newline(); flush stdout; Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout; print_newline()
                          | None -> Printf.printf "%s - Failed with reason: %s" (fst srcr) (snd srcr); print_newline(); flush stdout; print_newline()

let testTypeChecker =
  let parserAstList = List.map parseAst typeCheckerTests in
    List.map testAst parserAstList

let _ = testTypeChecker
