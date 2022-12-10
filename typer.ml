open Ast
open Typed_ast

exception Typing_Error of loc * string
let gfail loc msg = raise (Typing_Error(loc, msg))

module Smap = Map.Make(String)
module Sset = Set.Make(String)
type envid = Varid of var_ident * typ | Funid of fun_ident * ftyp
type tenv = envid Smap.t


let nbfun = ref 0
let funs = ref []


let lvalue e = match e.edesc with
| Ident _ -> true
| Unop (Deref, _) -> true
| _ -> false

let equiv u v = match u, v with
  | t1, t2 when t1 = t2 -> true (* réflexive *)
  | Int, Bool | Bool, Int -> true (* int = bool *)
  | Pointer t1, Pointer t2 when (t1 = Void || t2 = Void) -> true (* void* = t* *)
  | _ -> false

(* On substituant bool par int, equiv Int devient = Int ce qui*)
(* permet d'utiliser des filtres naturels : voir binop Plus et Minus*)
let bool_eradictor = function
  | Bool -> Int
  | t -> t
let be2 e1 e2 = (bool_eradictor e1.etyp, bool_eradictor e2.etyp)

let rec type_expr (env : tenv) typing =
  let aux = type_expr env in
  let fail = gfail typing.eloc in
  let f1t s t = fail (Format.sprintf s (typ_str t))
  and f2t s t1 t2 = fail (Format.sprintf s (typ_str t1) (typ_str t2))
  and fail_binop b t1 t2 = fail (Format.sprintf "invalid operands to binary %s (have '%s' and '%s')" (binop_str b) (typ_str t1) (typ_str t2))
  and f1s s x = fail (Format.sprintf s x)
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
      try begin
        match Smap.find s env with
        | Varid (id, t) -> T_Ident id, t
        | Funid _ -> f1s "identifier '%s' refers to a function (used as a variable)" s
      end with Not_found -> f1s "variable '%s' is undeclared" s
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
      if not (equiv tau1 tau2) then
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
      if not (equiv tau Int) then
        f1t "unary '+' operand must have arithmetic type (has type '%s')" tau;
      e_ty.t_edesc, Int

  | Unop(UMinus, e_raw) ->
    let e_ty = aux e_raw in
    let tau = e_ty.etyp in
    if not (equiv tau Int) then
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
      if not (equiv t1 t2) then
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


  (* Appel de fonction *)
  | Call (name, call_args) ->
    let id, proto_ret, proto_args = (try begin
      match Smap.find name env with
      | Varid _ -> f1s "identifier '%s' refers to a variable (used as a function)" name
      | Funid (id, t) -> id, fst t, snd t
    end with Not_found -> f1s "function '%s' is undeclared" name) in
    let nb_call = List.length call_args and nb_proto = List.length proto_args in
    if nb_call > nb_proto then
      f1s "too many arguments to function '%s'" name;
    if nb_call < nb_proto then
      f1s "too few arguments to function '%s'" name;
    let targs = List.map2 (fun e t ->
      let te = aux e in
      if not (equiv te.etyp t) then begin
        let msg = Format.sprintf "argument of type '%s' incompatible with expected type '%s'"
          (typ_str te.etyp) (typ_str t) in
        gfail e.eloc msg
      end;
      te
    ) call_args proto_args in
    T_Call (id, targs), proto_ret)


let rec type_stmt (env : tenv) expect in_loop (fpcur : int) fun_depth typing =
  let fail = gfail typing.sloc in
  let f2t s t1 t2 = fail (Format.sprintf s (typ_str t1) (typ_str t2))
  and aux_expr = type_expr env
  and aux_stmt = type_stmt env expect in

  match typing.sdesc with
  | Expr e_raw -> T_Expr (aux_expr e_raw), fpcur
  | Block b_raw ->
    let b_ty, fp_block = type_block env expect in_loop fpcur fun_depth b_raw in
    T_Block (b_ty), fp_block

  | Return None ->
      if expect <> Void then
        fail "expected a return value";
      T_Return None, fpcur
  | Return (Some e_raw) ->
      let e_ty = aux_expr e_raw in
      let tau = e_ty.etyp in
      if not (equiv expect tau) then
        f2t "return value doesn't have expected type %s (has type %s)" expect tau;
      T_Return (Some e_ty), fpcur

  | Cond (c_raw, s1_raw, s2_raw) ->
      let c_ty = aux_expr c_raw
      and s1_ty, fp1 = aux_stmt in_loop fpcur fun_depth s1_raw
      and s2_ty, fp2 = aux_stmt in_loop fpcur fun_depth s2_raw in

      if equiv c_ty.etyp Void then
        gfail c_raw.eloc "void value not ignored as it ought to be";
      T_Cond (c_ty, s1_ty, s2_ty), (max fp1 fp2)

  | Break ->
      if not in_loop then
        fail "break outside of loop";
      T_Break, fpcur
  | Continue ->
      if not in_loop then
        fail "continue outside of loop";
      T_Continue, fpcur

  | For (c_raw, es_raw, s_raw) ->
      let c_ty = aux_expr c_raw
      and es_ty = List.map aux_expr es_raw
      and s_ty, fp_body = aux_stmt true fpcur fun_depth s_raw in

      if equiv c_ty.etyp Void then
        gfail c_raw.eloc "void value not ignored as it ought to be";
      T_For (c_ty, es_ty, s_ty), fp_body

and type_block ?(be_init = Sset.empty) env_init expect in_loop fpover fun_depth raw_block =
  (* On a besoin de persistence uniquement pour rentrer dans *)
  (* un sous-bloc : dans ce cas appel récursif à type_block *)
  (* et type_decl aura accès à de nouvelles références *)
  let env = ref env_init in
  (* Set des noms du bloc actuel, à ne pas shadow *)
  (* Quand type_fct appelle type_block, il bloque les params *)
  let block_env = ref be_init in
  (* Comptage de variables *)
  (* On retient 1. le nombre de variables d'ordre 1 *)
  (* 2. le max des préfixes pour les sous-blocs *)
  let fpcur = ref fpover and max_prefix_fp = ref 0 in

  let type_decl typing =
    let fail = gfail (loc_decl typing) in
    let f2t s t1 t2 = fail (Format.sprintf s (typ_str t1) (typ_str t2)) in
    (* Empêche une redéfinition dans le même bloc *)
    let take name =
      if Sset.mem name !block_env then
        fail ("redefinition of '" ^ name ^ "'")
      else block_env := Sset.add name !block_env in
    
    match typing with
    | Stmt s -> begin
        let s_ty, sub_fp = type_stmt !env expect in_loop (-42) fun_depth s in
        max_prefix_fp := max !max_prefix_fp sub_fp;
        T_Stmt (s_ty)
      end
    | Var dv ->
        let ty, name, _ = dv.dv_var in
        if equiv ty Void then
          fail ("variable '" ^ name ^ "' declared void");

        take name;
        let init_ty = (match dv.dv_init with
        | None -> None
        | Some e_raw ->
          let e_ty = type_expr !env e_raw in
          if not (equiv ty e_ty.etyp) then
            f2t "incompatible types when initializing type '%s' using type '%s'" ty e_ty.etyp;
          Some e_ty) in
        (* On ajoute dans l'env *après* le typage de l'init *)
        (* pour éviter int x = f(x); *)
        fpcur := !fpcur + (sizeof ty);
        let vid = {
          offset = !fpcur;
          v_depth = fun_depth
        } in
        env := Smap.add name (Varid (vid, ty)) !env;
        T_Var { t_dv_typ = ty; t_dv_id = vid; t_dv_init = init_ty }

    | Fct df ->
        let name = df.df_id in
        take name;
        (* Ajouter son prototype dans l'env *avant* de la typer *)
        (* permettra de gérer la récursion correctement *)
        (* Elle pourra être shadow par une de ses fonctions imbriquées *)
        incr nbfun;
        let new_id = Format.sprintf "f_%d_%s" !nbfun name in
        env := Smap.add name (Funid (make_fid new_id (fun_depth + 1), ftyp_of_decl df)) !env;

        let typ_df = type_fct !env (fun_depth + 1) df in
        funs := typ_df :: !funs;
        T_Fct typ_df
  in
  List.map type_decl raw_block, !fpcur

and type_fct env fun_depth typing =
  let fun_id = make_fid (Format.sprintf "f_%d_%s" !nbfun typing.df_id) fun_depth in
  let parse_sig (curoff, df_env) param =
    let ty, name, ploc = param in
    if Smap.mem name df_env then
      gfail ploc ("redefinition of parameter '" ^ name ^ "'");
    let vid = {
      offset = curoff;
      v_depth = fun_depth
    } in
    curoff + (sizeof ty), Smap.add name (Varid (vid, ty)) df_env in

  let _, df_env = List.fold_left parse_sig (begin_offset_arguments, Smap.empty) typing.df_args in
  (* Chaque binding de df_env est ajouté dans var_env *)
  (* les paramètres shadow les variables globales *)
  let new_env = Smap.fold Smap.add df_env env in
  (* Les paramètres ne peuvent être shadow dans le bloc principal du corps *)
  (* d'où block_env initialisé à param_names (bloque les noms des paramètres) *)
  let param_names = Smap.fold (fun k _ a -> Sset.add k a) df_env Sset.empty in
  let block_ty, fp_body = type_block new_env typing.df_ret false (-42) fun_depth typing.df_body ~be_init:param_names in
  {
    t_df_ret = typing.df_ret;
    t_df_id = fun_id;
    t_df_args = List.map typ_of_var typing.df_args;
    t_df_body = block_ty;
    t_df_frame_size = fp_body;
  }

and type_prog (p_raw : prog) =
  let malloc = {
    df_ret = Pointer Void;
    df_id = "malloc";
    df_args = [Int, "n", dummy_loc];
    df_body = [];
    df_loc = dummy_loc
  }
  and putchar = {
    df_ret = Int;
    df_id = "putchar";
    df_args = [Int, "c", dummy_loc];
    df_body = [];
    df_loc = dummy_loc
  } in
  let user_main = try List.find (fun df -> df.df_id = "main") p_raw
    with Not_found -> gfail dummy_loc "missing main function" in
  if not (equiv Int user_main.df_ret) then
    gfail user_main.df_loc "function main must return int";
  if user_main.df_args <> [] then
    gfail user_main.df_loc "function main must have no parameters";
  let b_raw = List.map (fun df -> Fct df) (malloc :: putchar :: p_raw) in
  let b_ty, _ = type_block Smap.empty Void false 0 0 b_raw in
  List.map (function | T_Fct df -> df | _ -> failwith "impossible") b_ty
