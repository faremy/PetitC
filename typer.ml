open Ast
open Typed_ast

exception Typing_Error of loc * string

module Smap = Map.Make(String)
type venv = typ Smap.t and fenv = ftyp Smap.t

let lvalue = function
| Ident _ -> true
| Unop (Deref, _) -> true
| _ -> false


let type_expr var_env fct_env typing = 
  let fail msg =
    raise (Typing_Error (typing.eloc, msg)) in
  
  match typing.edesc with
  (* Constantes *)
  | Const Null -> { t_edesc = T_Const Null; etyp = Pointer Void }
  | Const (BoolCst b) -> { t_edesc = T_Const (BoolCst b); etyp = Bool }
  | Const (IntCst i) -> { t_edesc = T_Const (IntCst i); etyp = Int }

  (* Variables *)
  | Ident s -> begin
      try { t_edesc = T_Ident s; etyp = Smap.find s var_env }
      with Not_found -> fail (Format.sprintf "variable '%s' is undeclared" s)
    end

  (* sizeof *)
  | Sizeof Void -> fail "void has no size"
  | Sizeof t -> { t_edesc = T_Sizeof t; etyp = Int}

  (* Pointeur : amp et deref *)
  (* Assignation lv = rv, ex. x = (y = 2) *)
  (* Unop sur lvalue *)
  (* Unop numérique *)
  (* Binop numérique *)
  (* Avancer/reculer un pointeur, + à symétriser *)
  (* Distance entre deux pointeurs *)
  (* Appel de fonction *)
  | _ -> fail "cas non gere"
