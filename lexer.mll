{
	open Lexing
	open Parser
}

let digits = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z']

rule token = parse
|	'\n' { new_line lexbuf; token lexbuf }
|	[' ' '\t'] { token lexbuf }
|	eof { EOF }
|	_ { assert false }