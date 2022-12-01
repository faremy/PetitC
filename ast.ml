open Lexing
type loc = position * position

let pp_loc fmt (l : loc) =
  let (b, e) = l in
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  Format.fprintf fmt "l%d c%d-%d" l fc lc

type typ = Void | Int | Bool | Pointer of typ

let rec typ_str = function
  | Void -> "void"
  | Int -> "int"
  | Bool -> "bool"
  | Pointer tau -> (typ_str tau) ^ "*"

let pp_typ fmt (tau : typ) =
  Format.fprintf fmt "%s" (typ_str tau)

type const = IntCst of int | BoolCst of bool | Null

and unop = Incr of bool | Decr of bool | Amp | Not | Deref | UPlus | UMinus
(* Le booléen indique s'il faut renvoyer la valeur après [1] ou avant [0] incrémentation *)
and binop =
| Eq | Neq | Lt | Le | Gt | Ge
| And | Or
| Plus | Minus | Mul | Div | Mod

and var = typ * string
and decl_fct = {
  df_ret: typ;
  df_id: string;
  df_args: var list;
  df_body: decl list;
  df_loc: loc
}
and decl_var = {
  dv_var: var;
  dv_init: expr option;
  dv_loc: loc
}
and decl = Fct of decl_fct | Var of decl_var | Stmt of stmt

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
| Cond of expr * stmt * stmt (* pas de else = nothing *)
| For of expr * (expr list) * stmt
| Return of expr option
| Break
| Continue
and stmt = { sdesc: stmt_desc; sloc: loc }

and prog = decl_fct list
[@@deriving show {with_path = false}]

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

let dummy_stmt desc =
  {sdesc = desc; sloc = dummy_loc}

let dummy_expr desc =
  {edesc = desc; eloc = dummy_loc}

let nothing = Block [] (* un ; seul *)
let ctrue = Const (BoolCst true)
let cfalse = Const (BoolCst false)
let c0 = Const (IntCst 0)

let make_df (t, id, args, d, lc) = {
  df_ret = t;
  df_id = id;
  df_args = args;
  df_body = d;
  df_loc = lc
}

let make_dv (v, e, lc) = {
  dv_var = v;
  dv_init = e;
  dv_loc = lc
}

let binop_str = function
| Plus -> "+"
| Minus -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "%"
| And -> "&&"
| Or -> "||"
| Eq -> "=="
| Neq -> "!="
| Lt -> "<"
| Le -> "<="
| Gt -> ">"
| Ge -> ">="

let loc_decl = function
| Stmt s -> s.sloc
| Var v -> v.dv_loc
| Fct f -> f.df_loc
