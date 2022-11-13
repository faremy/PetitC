open Ast

let compile file =
  try let lexbuf = Lexing.from_channel (open_in file) in while Lexer.token lexbuf <> Parser.EOF do (); done
  with Lexer.Lexing_Error (s, l) -> Format.eprintf "File \"%s\", line %d, characters %d-%d:\nLexing error: %s\n"
    file l.line l.lcol l.rcol s

let () =
  Arg.parse [] compile ""