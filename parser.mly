%token EOF
%token<Ast.loc> COMMA, SEMICOLON
%token<Ast.loc> LPAR, RPAR, LBRA, RBRA, LSQR, RSQR (* (, ), {, }, [, ] *)
%token<Ast.loc> ASSIGN (* = *)
%token<Ast.loc> AND, OR (* &&, || *)
%token<Ast.loc> EQ, NEQ, LT, LE, GT, GE (* ==, !=, <, <=, >, >= *)
%token<Ast.loc> PLUS, MINUS, MUL, DIV, MOD (* +, -, *, /, % *)
%token<Ast.loc> NOT, AMP (* !, & *)
%token<Ast.loc> PPLUS, MMINUS (* ++, -- *)
%token<Ast.loc> TBOOL, TINT, TVOID
%token<Ast.loc> IF, ELSE, FOR, WHILE, CONTINUE, BREAK, RETURN
%token<Ast.loc> SIZEOF
%token<Ast.loc> NULL
%token<Ast.loc> TRUE, FALSE
%token<int * Ast.loc> INT
%token<string * Ast.loc> IDENT
%token<string * Ast.loc> INCLUDE

%type<unit> prog
%start prog
%%

prog:
	EOF { () }