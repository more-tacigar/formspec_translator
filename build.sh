rm *.cmi *.cmo parser.ml lexer.ml

ocamlc -c ast.ml
ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c translater.ml
ocamlc -c main.ml
ocamlc -o translater.native lexer.cmo parser.cmo ast.cmo translater.cmo main.cmo
