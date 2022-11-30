open Ast
open Typed_ast

exception Typing_Error of loc * string

module Smap = Map.Make(String)
type venv = typ Smap.t and fenv = ftyp Smap.t

let lvalue e = match e.edesc with
| Ident _ -> true
| Unop (Deref, _) -> true
| _ -> false

let equiv = function
  | t1, t2 when t1 = t2 -> true (* réflexive *)
  | Int, Bool | Bool, Int -> true (* int = bool *)
  | Pointer t1, Pointer t2 when (t1 = Void || t2 = Void) -> true (* void* = t* *)
  | _ -> false


let type_expr var_env fct_env =
  let rec aux typing =
    let fail msg =
      raise (Typing_Error (typing.eloc, msg)) in
    let f1t s t = fail (Format.sprintf s (typ_str t))
    and f2t s t1 t2 = fail (Format.sprintf s (typ_str t1) (typ_str t2))
    and require_lval e msg =
      if (not (lvalue e)) then fail ("lvalue required as " ^ msg) in
    
    (* Le match retourne une paire brute, transformée en record par make_te *)
    make_te (match typing.edesc with

    (* Constantes *)
    | Const Null -> T_Const Null, Pointer Void
    | Const (BoolCst b) -> T_Const (BoolCst b), Bool
    | Const (IntCst i) -> T_Const (IntCst i), Int

    (* Variables *)
    | Ident s -> begin
        try T_Ident s, Smap.find s var_env
        with Not_found -> fail (Format.sprintf "variable '%s' is undeclared" s)
      end

    (* Sizeof *)
    | Sizeof Void -> fail "void has no size"
    | Sizeof t -> T_Sizeof t, Int

    (* Pointeur : amp et deref *)
    | Unop(Amp, e_raw) -> begin
        require_lval e_raw "unary '&' operand";
        let e_ty = aux e_raw in
        T_Unop(Amp, e_ty), Pointer e_ty.etyp
      end

    | Unop(Deref, e_raw) -> begin
        let e_ty = aux e_raw in
        match (e_ty.etyp) with
          | Pointer Void -> fail "dereferencing 'void*' pointer"
          | Pointer tau -> T_Unop(Deref, e_ty), tau
          | tau -> f1t "invalid type argument of unary '*' (have '%s')" tau
      end

    (* Assignation lv = rv, ex. x = (y = 2) *)
    | Assign(e1r, e2r) -> begin
        require_lval e1r "left operand of assignment";
        let e1t = aux e1r and e2t = aux e2r in
        let tau1 = e1t.etyp and tau2 = e2t.etyp in
        if (not (equiv (tau1, tau2))) then
          f2t "incompatible types when assigning to type '%s' from type '%s'" tau1 tau2;
        T_Assign(e1t, e2t), e1t.etyp
      end
  
    (* Incr/decr/not *)
    | Unop(Incr _ as op, e_raw) | Unop(Decr _ as op, e_raw) -> begin
        require_lval e_raw (match op with
          | Incr _ -> "increment operand"
          | Decr _ -> "decrement operand"
          | _ -> fail "pas possible");
        let e_ty = aux e_raw in
        T_Unop(op, e_ty), e_ty.etyp
      end

    | Unop(Not, e_raw) ->
        let e_ty = aux e_raw in
        if (e_ty.etyp = Void) then
          fail "invalid use of void expression"
        else
          T_Unop(Not, e_ty), Int

    (* Unop numérique *)
    (* Binop numérique *)
    (* Avancer/reculer un pointeur, + à symétriser *)
    (* Distance entre deux pointeurs *)
    (* Appel de fonction *)
    | _ -> fail "cas non gere") in
  aux;;
