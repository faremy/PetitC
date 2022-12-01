open Ast

type t_decl_fct = {
  t_df_ret: typ;
  t_df_id: string;
  t_df_args: var list;
  t_df_body: t_decl list;
}
and t_decl_var = {
  t_dv_var: var;
  t_dv_init: t_expr option;
}
and t_decl = T_Fct of t_decl_fct | T_Var of t_decl_var | T_Stmt of t_stmt

and t_expr_desc =
| T_Const of const
| T_Ident of string
| T_Call of string * t_expr list
| T_Unop of unop * t_expr
| T_Binop of binop * t_expr * t_expr
| T_Assign of t_expr * t_expr
| T_Sizeof of typ
and t_expr = { t_edesc: t_expr_desc; etyp: typ }

and t_stmt =
| T_Expr of t_expr
| T_Block of t_decl list
| T_Cond of t_expr * t_stmt * t_stmt
| T_For of t_expr * t_expr list * t_stmt
| T_Return of t_expr option
| T_Break
| T_Continue

and t_prog = t_decl_fct list

let make_te (desc, tau) =
  { t_edesc = desc; etyp = tau }

let t_nothing = T_Block []
