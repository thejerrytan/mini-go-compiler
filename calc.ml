(* File calc.ml *)
open Go

let _ =
  try
    let lexbuf = Lexing.from_string "{a := 2; a = 4}" in
    while true do
      let result = Parser.prog Lexer.token lexbuf in
        Printf.printf "%s" (pretty_print_prog result); print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0