{
	open Lexing
	open Parser

	exception Lexing_Error of string

	let nl = ref true (* indique si l'on est ou non au début de la ligne *)
}

let digits = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']
let char = ['\032'-'\127']
let space = [' ' '\t']

rule token = parse
|	'\n' { new_line lexbuf; nl := true; token lexbuf }
|	space { token lexbuf }
|	"//" { l_comment lexbuf }
|	"/*" { comment lexbuf }
|	'#' {
	if not !nl then
		raise (Lexing_Error ("stray '#' in program"));
	read_include lexbuf
}

|	',' { nl := false; COMMA }
|	';' { nl := false; SEMICOLON }

|	'(' { nl := false; LPAR }
|	')' { nl := false; RPAR }
|	'{' { nl := false; LBRA }
|	'}' { nl := false; RBRA }
|	'[' { nl := false; LSQR }
|	']' { nl := false; RSQR }

|	'='	{ nl := false; ASSIGN }

|	"&&" { nl := false; AND }
|	"||" { nl := false; OR }

|	"==" { nl := false; EQ }
|	"!=" { nl := false; NEQ }
|	'<' { nl := false; LT }
|	"<=" { nl := false; LE }
|	'>' { nl := false; GT }
|	">=" { nl := false; GE }

|	'+' { nl := false; PLUS }
|	'-' { nl := false; MINUS }
|	'*' { nl := false; MUL }
|	'/' { nl := false; DIV }
|	'%' { nl := false; MOD }

|	'!' { nl := false; NOT }
|	'&' { nl := false; AMP }

|	"++" { nl := false; PPLUS }
|	"--" { nl := false; MMINUS }

|	"bool" { nl := false; TBOOL }
|	"int" { nl := false; TINT }
|	"void" { nl := false; TVOID }

|	"if" { nl := false; IF }
|	"else" { nl := false; ELSE }
|	"for" { nl := false; FOR }
|	"while" { nl := false; WHILE }
|	"continue" { nl := false; CONTINUE }
|	"break" { nl := false; BREAK }
|	"return" { nl := false; RETURN }

|	"sizeof" { nl := false; SIZEOF }

|	"NULL" { nl := false; NULL }

|	"true" { nl := false; TRUE }
|	"false" { nl := false; FALSE }

|	'0' | ['1'-'9'] digits* as i { nl := false; INT (int_of_string i) }
|	'\'' (char # ['\'' '\\'] as c) '\'' { nl := false; INT (Char.code c) }
|	"\'\\\'\'" { nl := false; INT (Char.code '\'') }
|	"\'\\\\\'" { nl := false; INT (Char.code '\\') }
|	"\'\\n\'" { nl := false; INT (Char.code '\n') }
|	"\'\\t\'" { nl := false; INT (Char.code '\t') }

|	('_' | alpha) ('_' | alpha | digits)* as s { nl := false; IDENT s }

|	eof { EOF }
|	_ { raise (Lexing_Error ("illegal character")) }
and l_comment = parse
|	'\n' { new_line lexbuf; nl := true; token lexbuf }
|	eof { EOF }
|	_ { l_comment lexbuf }
and comment = parse
|	'\n' { new_line lexbuf; comment lexbuf }
	(* gcc ne prend pas en compte les retours à la ligne dans les commentaires (pas besoin de modifier nl) *)
|	eof { raise (Lexing_Error ("unfinished comment")) }
|	"*/" { token lexbuf }
|	_ { comment lexbuf }
and read_include = parse
|	"include" space* '<' ((char # '>')* as f) '>' space* '\n' { new_line lexbuf; INCLUDE f }
|	"include" space* { raise (Lexing_Error ("invalid include argument")) }
|	_ { raise (Lexing_Error ("stray '#' in program")) }
