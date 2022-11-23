%{
	open Ast
	let check_include = List.iter (function
		| "stdlib.h" | "stdbool.h" | "stdio.h" -> ()
		| _ -> failwith "unknown include file"
	)
%}

%token EOF
%token COMMA, SEMICOLON
%token LPAR, RPAR, LBRA, RBRA, LSQR, RSQR (* (, ), {, }, [, ] *)
%token ASSIGN (* = *)
%token AND, OR (* &&, || *)
%token EQ, NEQ, LT, LE, GT, GE (* ==, !=, <, <=, >, >= *)
%token PLUS, MINUS, MUL, DIV, MOD (* +, -, *, /, % *)
%token NOT, AMP (* !, & *)
%token PPLUS, MMINUS (* ++, -- *)
%token TBOOL, TINT, TVOID (* types *)
%token IF, ELSE, FOR, WHILE, CONTINUE, BREAK, RETURN
%token SIZEOF
%token NULL
%token TRUE, FALSE
%token<int> INT
%token<string> IDENT
%token<string> INCLUDE

%right ASSIGN
%left OR
%left AND
%left EQ, NEQ
%left LT, LE, GT, GE
%left PLUS, MINUS
%left MUL, DIV, MOD
%right AMP, NOT, PPLUS, MMINUS
%nonassoc LSQR
%nonassoc IF
%nonassoc ELSE

%type<Ast.prog> prog
%start prog
%%

%inline binop:
|	EQ { Eq }
|	NEQ { Neq }
|	LT { Lt }
|	LE { Le }
|	GT { Gt }
|	GE { Ge }
|	AND { And }
|	OR { Or }
|	PLUS { Plus }
|	MINUS { Minus }
|	MUL { Mul }
|	DIV { Div }
|	MOD { Mod }

typ:
|	TINT { Int }
|	TBOOL { Bool }
|	TVOID { Void }
|	t = typ; MUL { Pointer t }
var:
|	t = typ; id = IDENT; { (t, id) }

expr_desc:
|	i = INT { Const (IntCst i) }
|	TRUE { Const (BoolCst true) }
|	FALSE { Const (BoolCst false) }
|	NULL { Const Null }
|	id = IDENT { Ident id }
	
|	id = IDENT; LPAR; e = separated_list(COMMA, expr); RPAR { Call (id, e) }
|	SIZEOF; LPAR; t = typ; RPAR { Sizeof t }

|	PLUS; e = expr { e.edesc } %prec PPLUS
|	MINUS; e = expr { Binop (Minus, { edesc = Const (IntCst 0); eloc = $loc }, e) } %prec MMINUS
|	PPLUS; e = expr { Unop (Incr false, e) }
|	MMINUS e = expr { Unop (Decr false, e) }
|	e = expr; PPLUS { Unop (Incr true, e) }
|	e = expr; MMINUS { Unop (Decr true, e) }

|	AMP; e = expr { Unop (Amp, e) }
|	NOT; e = expr { Unop (Not, e) }
|	MUL; e = expr { Unop (Deref, e) } %prec AMP
|	e1 = expr; LSQR; e2 = expr; RSQR { Unop (Deref, { edesc = Binop (Plus, e1, e2); eloc = $loc }) }

|	e1 = expr; op = binop; e2 = expr { Binop (op, e1, e2) }
|	e1 = expr; ASSIGN; e2 = expr { Assign (e1, e2) }

|	LPAR; e = expr; RPAR { e.edesc }
expr:
|	e = expr_desc { { edesc = e; eloc = $loc } }

stmt_desc:
|	SEMICOLON { nothing }
|	e = expr; SEMICOLON { Expr e }
|	IF; LPAR; e = expr; RPAR; s1 = stmt { Cond (e, s1, dummy_stmt nothing) } %prec IF
|	IF; LPAR; e = expr; RPAR; s1 = stmt; ELSE; s2 = stmt { Cond (e, s1, s2) }
stmt:
|	s = stmt_desc { { sdesc = s; sloc = $loc } }

block:
|	LBRA b = list(decl) RBRA { b }
decl_var:
|	v = var { make_dv (v, None, $loc) }
|	v = var; ASSIGN; e = expr { make_dv (v, Some e, $loc) }
decl_fct:
|	t = typ; id = IDENT; LPAR; arg = separated_list(COMMA, var); RPAR; b = block { make_df (t, id, arg, b, $loc) }
decl:
|	d = decl_var; SEMICOLON { Var d }
|	s = stmt { Stmt s }

incl:
|	s = INCLUDE { s }
prog:
|	i = list(incl); f = list(decl_fct); EOF { check_include i; f }
