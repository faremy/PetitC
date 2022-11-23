open Ast
open Typed_ast

exception Typing_Error of loc * string

module Smap = Map.Make(String)
type venv = typ Smap.t and fenv = ftyp Smap.t

let lvalue e = match e.edesc with
| Ident _ -> true
| Unop (Deref, _) -> true
| _ -> false


let type_expr var_env fct_env =
  let rec aux typing =
    let fail msg =
      raise (Typing_Error (typing.eloc, msg)) in
    let require_lval e msg =
      if (not (lvalue e)) then fail msg in
    
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
    | Unop(Amp, e_raw) -> begin
        require_lval e_raw "lvalue required as unary '&' operand";
        let e_ty = aux e_raw in
        {t_edesc = T_Unop(Amp, e_ty); etyp = Pointer e_ty.etyp}
      end

    | Unop(Deref, e_raw) -> begin
        let e_ty = aux e_raw in
        match (e_ty.etyp) with
          | Pointer tau -> {t_edesc = T_Unop(Deref, e_ty); etyp = tau}
          | tau -> fail (Format.sprintf "invalid type argument of unary '*' (have '%s')" (typ_str tau))
      end

    (* Assignation lv = rv, ex. x = (y = 2) *)
    (* Unop sur lvalue *)
    (* Unop numérique *)
    (* Binop numérique *)
    (* Avancer/reculer un pointeur, + à symétriser *)
    (* Distance entre deux pointeurs *)
    (* Appel de fonction *)
    | _ -> fail "cas non gere" in
  aux;;
