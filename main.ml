let main () =
  let filename = Sys.argv.(1) in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let formspec = Parser.formspec Lexer.read lexbuf in
  let translater = new Translater.translater in
  let result = translater#translate_formspec formspec in
  print_string (Buffer.contents result)

let () = main ()
