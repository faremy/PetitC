let compile file =
  try let lexbuf = Lexing.from_channel (open_in file) in while Lexer.token lexbuf <> Parser.EOF do (); done
  with Lexer.Lexing_Error s -> Format.eprintf "Lexing error: %s\n" s

let () =
  Arg.parse [] compile ""