type loc = Lexing.position * Lexing.position

and typ = Void | Int | Bool | Pointer of typ
and const = IntCst of int | BoolCst of bool | Null

and unop = Incr of bool | Decr of bool | Amp | Not | Deref
(* Le booléen indique s'il faut renvoyer la valeur après [1] ou avant [0] incrémentation *)
and binop =
| Eq | Neq | Lt | Le | Gt | Ge
| And | Or
| Plus | Minus | Mul | Div | Mod

and var = typ * string
and decl_fct = var * var list * decl list
and decl_var = var * expr option
and decl = Var of decl_var | Stmt of stmt

and expr_desc =
| Const of const
| Ident of string
| Call of string * expr list
| Unop of unop * expr
| Binop of binop * expr * expr
| Assign of expr * expr
| Sizeof of typ
and expr = { edesc: expr_desc; eloc: loc }

and stmt_desc =
| Expr of expr
| Block of decl list
| Cond of expr * stmt * stmt
| While of expr * stmt
| Return of expr option
| Break
| Continue
and stmt = { sdesc: stmt_desc; sloc: loc }

and prog = decl_fct list