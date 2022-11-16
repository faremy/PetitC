
(* Programme principal *)

open Format
open Lexing
open Parser
open Ast

let usage = "usage: petitc [options] file.c"

let parse_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".c") then
      raise (Arg.Bad "no .c extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in

  let rec dumb_parser () =
    match (Lexer.token lb) with
    | Parser.EOF -> ()
    | t -> if (!Lexer.nl) then printf "\n"; printf "%s " (Util.token_to_string t); dumb_parser () in
  
  try
    dumb_parser ();
    (* let f = Parser.file tok in  *)
    (* Format.printf "%a@." pp_file f;  *)
    if !parse_only then exit 0;
    (*Interp.file f*)
  with
    | Lexer.Lexing_Error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2