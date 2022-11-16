%token EOF
%token COMMA, SEMICOLON
%token LPAR, RPAR, LBRA, RBRA, LSQR, RSQR (* (, ), {, }, [, ] *)
%token ASSIGN (* = *)
%token AND, OR (* &&, || *)
%token EQ, NEQ, LT, LE, GT, GE (* ==, !=, <, <=, >, >= *)
%token PLUS, MINUS, MUL, DIV, MOD (* +, -, *, /, % *)
%token NOT, AMP (* !, & *)
%token PPLUS, MMINUS (* ++, -- *)
%token TBOOL, TINT, TVOID
%token IF, ELSE, FOR, WHILE, CONTINUE, BREAK, RETURN
%token SIZEOF
%token NULL
%token TRUE, FALSE
%token<int> INT
%token<string> IDENT
%token<string> INCLUDE

%type<unit> prog
%start prog
%%

prog:
	EOF { () }