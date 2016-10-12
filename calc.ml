(* File calc.ml *)
open Go

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.prog Lexer.token lexbuf in
        pretty_print result; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0