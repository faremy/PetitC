%{
	open Ast
	let check_include loc = function
		| "stdlib.h" | "stdbool.h" | "stdio.h" -> ()
		| f -> raise (Invalid_Include (Format.sprintf "unknown include file %s" f, loc))
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
|	t = typ; id = IDENT; { (t, id, $loc) }

expr_desc:
|	i = INT { Const (IntCst i) }
|	TRUE { ctrue }
|	FALSE { cfalse }
|	NULL { Const Null }
|	id = IDENT { Ident id }
	
|	id = IDENT; LPAR; e = separated_list(COMMA, expr); RPAR { Call (id, e) }
|	SIZEOF; LPAR; t = typ; RPAR { Sizeof t }

|	PLUS; e = expr { Unop (UPlus, e) } %prec PPLUS
|	MINUS; e = expr { Unop (UMinus, e) } %prec MMINUS
|	PPLUS; e = expr { Unop (Incr true, e) }
|	MMINUS e = expr { Unop (Decr true, e) }
|	e = expr; PPLUS { Unop (Incr false, e) }
|	e = expr; MMINUS { Unop (Decr false, e) }

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
|	WHILE; LPAR; e = expr; RPAR; s = stmt { For (e, [], s) }
|	FOR; LPAR; v = option(decl_var); SEMICOLON; e = option(expr); SEMICOLON; es = separated_list(COMMA, expr); RPAR; s = stmt
	{
		let cond = Option.value e ~default:(dummy_expr ctrue) in
		match v with
		|	None -> For (cond, es, s)
		|	Some dv -> Block [ Var dv; Stmt { sdesc = For (cond, es, s); sloc = $loc } ]
	}
|	b = block { Block b }
|	RETURN; e = option(expr); SEMICOLON { Return e }
|	BREAK; SEMICOLON { Break }
|	CONTINUE; SEMICOLON { Continue }
stmt:
|	s = stmt_desc { { sdesc = s; sloc = $loc } }

block:
|	LBRA b = list(decl) RBRA { b }
decl_var:
|	v = var { make_dv (v, None, $loc) }
|	v = var; ASSIGN; e = expr { make_dv (v, Some e, $loc) }
%inline decl_fct_sig:
|	t = typ; id = IDENT; LPAR; arg = separated_list(COMMA, var); RPAR; { (t, id, arg, $loc) }
decl_fct:
|	s = decl_fct_sig; b = block { make_df s b }
decl:
|	d = decl_fct { Fct d }
|	d = decl_var; SEMICOLON { Var d }
|	s = stmt { Stmt s }

incl:
|	s = INCLUDE { check_include $loc s }
prog:
|	list(incl); f = list(decl_fct); EOF { f }
