open Ast
open Typed_ast

exception Typing_Error of loc * string

module Smap = Map.Make(String)
type venv = typ Smap.t and fenv = ftyp Smap.t

let lvalue = function
| Ident _ -> true
| Unop (Deref, _) -> true
| _ -> false


let type_expr var_env fct_env e = 
  let fail msg =
    raise (Typing_Error (e.eloc, msg)) in
  
  match e.edesc with
  | Const Null -> { t_edesc = T_Const Null; etyp = Void }
  | Const (BoolCst b) -> { t_edesc = T_Const (BoolCst b); etyp = Bool }
  | Const (IntCst i) -> { t_edesc = T_Const (IntCst i); etyp = Int }
  | Ident s -> begin
    try { t_edesc = T_Ident s; etyp = Smap.find s var_env }
    with Not_found -> fail (Format.sprintf "variable %s is undefined" s)
  end
  | Sizeof Void -> fail "void has no size"
  | Sizeof t -> { t_edesc = T_Sizeof t; etyp = Int}
  | _ -> fail "cas non gere"
