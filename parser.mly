%{
%}

%token EOF

%type<unit> prog
%start prog
%%

prog:
	EOF { () }