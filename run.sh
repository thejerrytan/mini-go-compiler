ocamllex lexer.mll       # generates lexer.ml
ocamlyacc parser.mly     # generates parser.ml and parser.mli
ocamlc -c -g go.ml
ocamlc -c -g parser.mli
ocamlc -c -g parser.ml
ocamlc -c -g lexer.ml
# ocamlc -c -g ty.ml
ocamlc -c -g normalize.ml
ocamlc -c -g intermediate.ml
ocamlc -c -g vm.ml
ocamlc -c -g calc.ml
ocamlc -g -o calc go.cmo lexer.cmo parser.cmo normalize.cmo intermediate.cmo vm.cmo calc.cmo
