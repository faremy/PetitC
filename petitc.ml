
(* Programme principal *)

open Format
open Lexing
open Typed_ast

let usage = "usage: petitc [options] file.c"

let parse_only = ref false
let type_only = ref false
let print_ast = ref false
let print_typ_ast = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only", Arg.Set type_only, "  stop after typing";
    "--debug-alloc", Arg.Set Typer.debug_alloc, "  print offset and depths (var, args, fcts)";
    "--print-ast", Arg.Set print_ast, "  print raw ast (ppx_deriving.show)";
    "--print-typed-ast", Arg.Set print_typ_ast, "  print typed ast (ppx_deriving.show)"
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
  
  try
    let ast = Parser.prog Lexer.token lb in
    if !print_ast then
      Format.eprintf "%a@." Ast.pp_prog ast;
    if !parse_only then exit 0;

    let t_ast = Typer.type_prog ast in
    if !print_typ_ast then
      Format.eprintf "%a@." pp_t_prog t_ast;
    if !type_only then exit 0;
    let prog = Compile.compile_prog t_ast !Typer.funs in
    let file_s = (Filename.chop_suffix file ".c") ^ ".s" in
    let f = open_out file_s in
    let fmt = formatter_of_out_channel f in
    X86_64.print_program fmt prog;
    fprintf fmt "@?";
    close_out f
  with
  | Lexer.Lexing_Error s ->
    report (lexeme_start_p lb, lexeme_end_p lb);
    eprintf "Lexical error: %s@." s;
    exit 1
  | Parser.Error ->
    report (lexeme_start_p lb, lexeme_end_p lb);
    eprintf "Syntax error@.";
    exit 1
  | Ast.Invalid_Include (s, l) ->
    report l;
    eprintf "Invalid include: %s@." s;
    exit 1
  | Typer.Typing_Error (l, s) ->
    report l;
    eprintf "error: %s@." s;
    exit 1
  | e ->
    eprintf "Anomaly: %s\n@." (Printexc.to_string e);
    exit 2
