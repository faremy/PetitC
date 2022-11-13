{
	open Lexing
	open Parser
	open Ast

	exception Lexing_Error of string * loc

	let get_loc s pos = {
		line = pos.pos_lnum;
		lcol = pos.pos_cnum - pos.pos_bol + 1;
		rcol = pos.pos_cnum - pos.pos_bol + String.length s;
	}

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
|	"/*" { comment (get_loc "/*" lexbuf.lex_start_p) lexbuf }
|	'#' {
	let l = get_loc "#" lexbuf.lex_start_p in
	if not !nl then
		raise (Lexing_Error ("stray '#' in program", l));
	read_include l lexbuf
}

|	',' { nl := false; COMMA (get_loc "," lexbuf.lex_start_p) }
|	';' { nl := false; SEMICOLON (get_loc ";" lexbuf.lex_start_p) }

|	'(' { nl := false; LPAR (get_loc "(" lexbuf.lex_start_p) }
|	')' { nl := false; RPAR (get_loc ")" lexbuf.lex_start_p) }
|	'{' { nl := false; LBRA (get_loc "{" lexbuf.lex_start_p) }
|	'}' { nl := false; RBRA (get_loc "}" lexbuf.lex_start_p) }
|	'[' { nl := false; LSQR (get_loc "[" lexbuf.lex_start_p) }
|	']' { nl := false; RSQR (get_loc "]" lexbuf.lex_start_p) }

|	'='	{ nl := false; ASSIGN (get_loc "=" lexbuf.lex_start_p) }

|	"&&" { nl := false; AND (get_loc "&&" lexbuf.lex_start_p) }
|	"||" { nl := false; OR (get_loc "||" lexbuf.lex_start_p) }

|	"==" { nl := false; EQ (get_loc "==" lexbuf.lex_start_p) }
|	"!=" { nl := false; NEQ (get_loc "!=" lexbuf.lex_start_p) }
|	'<' { nl := false; LT (get_loc "<" lexbuf.lex_start_p) }
|	"<=" { nl := false; LE (get_loc "<=" lexbuf.lex_start_p) }
|	'>' { nl := false; GT (get_loc ">" lexbuf.lex_start_p) }
|	">=" { nl := false; GE (get_loc ">=" lexbuf.lex_start_p) }

|	'+' { nl := false; PLUS (get_loc "+" lexbuf.lex_start_p) }
|	'-' { nl := false; MINUS (get_loc "-" lexbuf.lex_start_p) }
|	'*' { nl := false; MUL (get_loc "*" lexbuf.lex_start_p) }
|	'/' { nl := false; DIV (get_loc "/" lexbuf.lex_start_p) }
|	'%' { nl := false; MOD (get_loc "%" lexbuf.lex_start_p) }

|	'!' { nl := false; NOT (get_loc "!" lexbuf.lex_start_p) }
|	'&' { nl := false; AMP (get_loc "&" lexbuf.lex_start_p) }

|	"++" { nl := false; PPLUS (get_loc "++" lexbuf.lex_start_p) }
|	"--" { nl := false; MMINUS (get_loc "--" lexbuf.lex_start_p) }

|	"bool" { nl := false; TBOOL (get_loc "bool" lexbuf.lex_start_p) }
|	"int" { nl := false; TINT (get_loc "int" lexbuf.lex_start_p) }
|	"void" { nl := false; TVOID (get_loc "void" lexbuf.lex_start_p) }

|	"if" { nl := false; IF (get_loc "if" lexbuf.lex_start_p) }
|	"else" { nl := false; ELSE (get_loc "else" lexbuf.lex_start_p) }
|	"for" { nl := false; FOR (get_loc "for" lexbuf.lex_start_p) }
|	"while" { nl := false; WHILE (get_loc "while" lexbuf.lex_start_p) }
|	"continue" { nl := false; CONTINUE (get_loc "continue" lexbuf.lex_start_p) }
|	"break" { nl := false; BREAK (get_loc "break" lexbuf.lex_start_p) }
|	"return" { nl := false; RETURN (get_loc "return" lexbuf.lex_start_p) }

|	"sizeof" { nl := false; SIZEOF (get_loc "sizeof" lexbuf.lex_start_p) }

|	"NULL" { nl := false; NULL (get_loc "NULL" lexbuf.lex_start_p) }

|	"true" { nl := false; TRUE (get_loc "true" lexbuf.lex_start_p) }
|	"false" { nl := false; FALSE (get_loc "false" lexbuf.lex_start_p) }

|	'0' | ['1'-'9'] digits* as i { nl := false; INT (int_of_string i, get_loc i lexbuf.lex_start_p) }
|	'\'' (char # ['\'' '\\'] as c) '\'' as s { nl := false; INT (Char.code c, get_loc s lexbuf.lex_start_p) }
|	"\'\\\'\'" { nl := false; INT (Char.code '\'', get_loc "\'\\\'\'" lexbuf.lex_start_p) }
|	"\'\\\\\'" { nl := false; INT (Char.code '\\', get_loc "\'\\\\\'" lexbuf.lex_start_p) }
|	"\'\\n\'" { nl := false; INT (Char.code '\n', get_loc "\'\\n\'" lexbuf.lex_start_p) }
|	"\'\\t\'" { nl := false; INT (Char.code '\t', get_loc "\'\\t\'" lexbuf.lex_start_p) }

|	('_' | alpha) ('_' | alpha | digits)* as s { nl := false; IDENT (s, get_loc s lexbuf.lex_start_p) }

|	eof { EOF }
|	_ as c { raise (Lexing_Error ("illegal character", get_loc (String.make 1 c) lexbuf.lex_start_p)) }
and l_comment = parse
|	'\n' { new_line lexbuf; nl := true; token lexbuf }
|	eof { EOF }
|	_ { l_comment lexbuf }
and comment start = parse
|	'\n' { new_line lexbuf; comment start lexbuf }
	(* gcc ne prend pas en compte les retours à la ligne dans les commentaires (pas besoin de modifier nl) *)
|	eof { raise (Lexing_Error ("unfinished comment", start)) }
|	"*/" { token lexbuf }
|	_ { comment start lexbuf }
and read_include start = parse
|	"include" space* '<' ((char # '>')* as f) '>' space* '\n' { INCLUDE (f, start) }
|	"include" space* { raise (Lexing_Error ("invalid include argument", start)) }
|	_ { raise (Lexing_Error ("stray '#' in program", start)) }