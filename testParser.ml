(* File testParser.ml *)
open Go

let parser src =
  let lexbuf = Lexing.from_channel (open_in src) in
  try
    match (Parser.prog Lexer.token lexbuf) with
    | Go.Prog(procls, stmt) -> Some (Go.Prog(procls, stmt))
  with
    | Lexer.Eof -> None
    | Parsing.Parse_error -> None

let parserTests = [
  ("./tests/parserTests/parserSimpleValidProgram.go",
   "Valid simple main body program for parsing");
  ("./tests/parserTests/parserAnotherSimpleValidProgram.go",
   "Another valid simple main body program for parsing");
  ("./tests/parserTests/parserFunctionParsingTest.go",
   "Valid sample function declaration program for parsing");
  ("./tests/parserTests/parserWrongSemicolonSyntax.go",
   "invalid semicolon on last line");
  ("./tests/parserTests/parserDeadCodeAfterReturn.go",
   "dead code after a return on same block");
  ("./tests/parserTests/parserFunctionWithReturnTypeLastLineMustBeReturn.go",
   "the last statement of a function with a return type must be a return statement");
]

let parseAst srcr =
  let src = fst srcr in match (parser src) with
  | Some ast -> (srcr, ast)
  | None -> (srcr, Go.Prog ([], Skip))

let printAst (srcr, ast) = match ast with
  | Go.Prog ([], Skip) -> Printf.printf "%s - Failed with reason: %s" (fst srcr) (snd srcr); print_newline(); flush stdout; print_newline()
  | _ ->  Printf.printf "%s - %s" (fst srcr) (snd srcr); print_newline(); flush stdout; Printf.printf "%s" (print_prog 0 ast); print_newline(); flush stdout; print_newline()

let testParser =
  let parserAstList = List.map parseAst parserTests in
    List.map printAst parserAstList

let _ = testParser
