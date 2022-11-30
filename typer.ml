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

let bool_eradictor = function
  | Bool -> Int
  | t -> t
let be2 e1 e2 = (bool_eradictor e1.etyp, bool_eradictor e2.etyp)

let type_expr var_env fct_env =
  let rec aux typing =
    let fail msg =
      raise (Typing_Error (typing.eloc, msg)) in
    let f1t s t = fail (Format.sprintf s (typ_str t))
    and f2t s t1 t2 = fail (Format.sprintf s (typ_str t1) (typ_str t2))
    and fail_binop b t1 t2 = fail (Format.sprintf "invalid operands to binary %s (have '%s' and '%s')" (binop_str b) (typ_str t1) (typ_str t2))
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
          | tau -> f1t "argument of unary '*' is not a pointer (has type '%s')" tau
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
          fail "invalid use of void expression (not)";
        T_Unop(Not, e_ty), Int

    (* Unop arithmétiques *)
    | Unop(UPlus, e_raw) ->
        let e_ty = aux e_raw in
        let tau = e_ty.etyp in
        if not (equiv (tau, Int)) then
          f1t "unary '+' operand must have arithmetic type (has type '%s')" tau;
        e_ty.t_edesc, Int

    | Unop(UMinus, e_raw) ->
      let e_ty = aux e_raw in
      let tau = e_ty.etyp in
      if not (equiv (tau, Int)) then
        f1t "unary '-' operand must have arithmetic type (has type '%s')" tau;
      T_Binop (Minus, { t_edesc = T_Const (IntCst 0); etyp = Int }, e_ty), Int

    (* Binop comparison *)
    | Binop(Eq as op, e1r, e2r)
    | Binop(Neq as op, e1r, e2r)
    | Binop(Lt as op, e1r, e2r)
    | Binop(Le as op, e1r, e2r)
    | Binop(Gt as op, e1r, e2r)
    | Binop(Ge as op, e1r, e2r) ->
        let e1t = aux e1r and e2t = aux e2r in
        let t1 = e1t.etyp and t2 = e2t.etyp in
        if (not (equiv (t1, t2))) then
          fail_binop op t1 t2
        else if (e1t.etyp = Void) then
          fail "invalid use of void expression (comparison)"
        else
          T_Binop(op, e1t, e2t), Int

    (* + et - : int et pointeur *)
    | Binop(Plus as op, e1r, e2r)
    | Binop(Minus as op, e1r, e2r) ->
        let e1t = aux e1r and e2t = aux e2r in
        T_Binop(op, e1t, e2t), (match (be2 e1t e2t) with
          | Int, Int -> Int
          | Pointer t, Int -> Pointer t
          | Int, Pointer t when op = Plus -> Pointer t (* + symétrisé *)
          | Pointer t1, Pointer t2 when (op = Minus && t1 = t2) -> Int
          | _ -> fail_binop op e1t.etyp e2t.etyp)

    (* Binop {*,/,%,||,&&} *)
    | Binop(op, e1r, e2r) ->
        let e1t = aux e1r and e2t = aux e2r in
        if (be2 e1t e2t <> (Int, Int)) then
          fail_binop op e1t.etyp e2t.etyp
        else T_Binop(op, e1t, e2t), Int


    (* Distance entre deux pointeurs *)
    (* Appel de fonction *)
    | Call (name, args) ->
      let f = (try Smap.find name fct_env with Not_found -> fail (Format.sprintf "function '%s' is undeclared" name)) in
      let targs = List.map2 (fun e t ->
        let te = aux e in
        if not (equiv (te.etyp, t)) then
          raise (Typing_Error (e.eloc,
            Format.sprintf "argument of type '%s' incompatible with expected type '%s'" (typ_str te.etyp) (typ_str t)));
          (*Cannot use fail because localisation is the one of the argument*)
        te
      ) args f.args in
      T_Call (name, targs), f.ret) in
  aux
