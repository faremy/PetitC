open Ast
open Typed_ast
open X86_64


let fail () = failwith "impossible"
let new_control = let nb = ref 0 in fun () -> incr nb; !nb
let undef = "undef"

let rewind_rbp =
  let rec aux code = function
    | 0 -> code
    | n when n > 0 -> aux (code ++ movq (ind ~ofs:offset_rbp_parent rax) !%rax) (n - 1)
    | _ -> fail ()
  in aux (movq !%rbp !%rax)

let rec compile_lvalue expr =
  (* Pour une lvalue, renvoie l'adresse dans laquelle est stockée la valeur, au lieu de la valeur elle même *)
  match expr.t_edesc with
  | T_Ident { offset = o; v_depth = _ } -> movq !%rbp !%rax ++ addq (imm o) !%rax
  | T_Unop (Deref, e) -> compile_expr e
  | _ -> fail ()
and compile_expr expr =
  match expr.t_edesc with
  | T_Const (IntCst i) -> movq (imm i) !%rax
  | T_Const (BoolCst b) -> movq (imm (if b then 1 else 0)) !%rax
  | T_Const Null -> movq (imm 0) !%rax
  | T_Ident _ -> compile_lvalue expr ++ movq (ind rax) !%rax
  | T_Call (id, args) ->
    List.fold_left (fun code e -> compile_expr e ++ pushq !%rax ++ code) nop args
    ++ pushq (imm 0) (* TODO : rbp du parent *)
    ++ call id.name
    ++ popn (8 * (1 + List.length args))

  | T_Unop (UPlus, e) -> compile_expr e
  | T_Unop (UMinus, e) -> compile_expr e ++ negq !%rax
  | T_Unop (Deref, e) -> compile_expr e ++ movq (ind rax) !%rax
  | T_Unop (Amp, e) -> compile_lvalue e
  | T_Unop (Incr true as op, e)
  | T_Unop (Decr true as op, e) ->
    let offset = (match Typer.bool_eradictor expr.etyp with Pointer Void | Int -> 1 | _ -> 8) in
    compile_lvalue e
    ++ (match op with Incr _ -> addq | Decr _ -> subq | _ -> fail ()) (imm offset) (ind rax)
    ++ movq (ind rax) !%rax
  | T_Unop (Incr false as op, e)
  | T_Unop (Decr false as op, e) ->
    let offset = (match Typer.bool_eradictor expr.etyp with Pointer Void | Int -> 1 | _ -> 8) in
    compile_lvalue e
    ++ movq !%rax !%rbx
    ++ movq (ind rbx) !%rax
    ++ (match op with Incr _ -> addq | Decr _ -> subq | _ -> fail ()) (imm offset) (ind rbx)
  | T_Unop (Not, e) ->
    compile_expr e
    ++ testq !%rax !%rax
    ++ sete !%al
    ++ movzbq !%al rax

  | T_Binop (Plus, e1, e2) ->
    compile_expr e2
    ++ pushq !%rax (* On sauvegarde le calcul du 2e terme *)
    ++ compile_expr e1
    ++ popq rbx
    ++ (match Typer.be2 e1 e2 with
    | Int, Int | Pointer Void, Int | Int, Pointer Void -> addq !%rbx !%rax
    | Pointer _, Int -> shlq (imm 3) !%rbx ++ addq !%rbx !%rax
    | Int, Pointer _ -> shlq (imm 3) !%rax ++ addq !%rbx !%rax
    | _ -> fail ())

  | T_Binop (Minus, e1, e2) ->
    compile_expr e2
    ++ pushq !%rax (* On sauvegarde le calcul du 2e terme *)
    ++ compile_expr e1
    ++ popq rbx
    ++ (match Typer.be2 e1 e2 with
    | Pointer Void, Pointer Void | Int, Int | Pointer Void, Int -> subq !%rbx !%rax
    | Pointer _, Int -> shlq (imm 3) !%rbx ++ subq !%rbx !%rax
    | Pointer _, Pointer _ -> subq !%rbx !%rax ++ sarq (imm 3) !%rax
    | _ -> fail ())

  | T_Binop (Mul, e1, e2) -> 
    compile_expr e2
    ++ pushq !%rax (* On sauvegarde le calcul du 2e terme *)
    ++ compile_expr e1
    ++ popq rbx
    ++ imulq !%rbx !%rax

  | T_Binop (Div as op, e1, e2)
  | T_Binop (Mod as op, e1, e2) ->
    compile_expr e2
    ++ pushq !%rax (* On sauvegarde le calcul du 2e terme *)
    ++ compile_expr e1
    ++ popq rbx
    ++ cqto
    ++ idivq !%rbx
    ++ (match op with Div -> nop | Mod -> movq !%rdx !%rax | _ -> fail ())
  
  | T_Binop (Eq as op, e1, e2)
  | T_Binop (Neq as op, e1, e2)
  | T_Binop (Lt as op, e1, e2)
  | T_Binop (Le as op, e1, e2)
  | T_Binop (Gt as op, e1, e2)
  | T_Binop (Ge as op, e1, e2) ->
    compile_expr e2
    ++ pushq !%rax
    ++ compile_expr e1
    ++ popq rbx
    ++ cmpq !%rbx !%rax
    ++ (match op with Eq -> sete | Neq -> setne | Lt -> setl | Le -> setle | Gt -> setg | Ge -> setge | _ -> fail ()) !%al
    ++ movzbq !%al rax

  | T_Binop(And as op, e1, e2)
  | T_Binop (Or as op, e1, e2) ->
    let id = new_control () in
    let skip_andor, jumper = match op with
      | And -> (Format.sprintf "and_skip_%d" id), je
      | Or -> (Format.sprintf "or_skip_%d" id), jne
      | _ -> fail() in
    compile_expr e1
    ++ testq !%rax !%rax
    ++ jumper skip_andor
    ++ compile_expr e2
    ++ label skip_andor
    ++ testq !%rax !%rax
    ++ setne !%al
    ++ movzbq !%al rax

  | T_Assign (e1, e2) ->
    compile_expr e2
    ++ pushq !%rax (* On sauvegarde le calcul du 2e terme *)
    ++ compile_lvalue e1
    ++ popq rbx
    ++ movq !%rbx (ind rax)
    ++ movq !%rbx !%rax (* a = b renvoie la valeur de a après assignation, c'est à dire la valeur de b *)
  | T_Sizeof _ -> movq (imm 8) !%rax
  | _ -> fail ()

let rec compile_stmt brk ctn = function
  | T_Expr e -> compile_expr e
  | T_Block b -> compile_block brk ctn b

  | T_For (cond, step, body) ->
    let id = new_control () in
    let lab_body = Format.sprintf "loop_%d" id
    and lab_continue = Format.sprintf "loop_%d_continue" id
    and lab_cond = Format.sprintf "loop_%d_condition" id
    and lab_break = Format.sprintf "loop_%d_break" id
    in

    jmp lab_cond
    ++ label lab_body
    ++ compile_stmt lab_break lab_continue body
    ++ label lab_continue
    ++ List.fold_left (fun code e -> code ++ compile_expr e) nop step
    ++ label lab_cond
    ++ compile_expr cond
    ++ testq !%rax !%rax
    ++ jne lab_body
    ++ label lab_break
  
  | T_Cond (cond, body_if, body_else) ->
    let id = new_control () in
    let skip_if = Format.sprintf "if_skip_%d" id
    and skip_else = Format.sprintf "else_skip_%d" id
    in

    compile_expr cond
    ++ testq !%rax !%rax
    ++ je skip_if
    ++ compile_stmt brk ctn body_if
    ++ jmp skip_else
    ++ label skip_if
    ++ compile_stmt brk ctn body_else
    ++ label skip_else

  | T_Return e -> (match e with None -> nop | Some v -> compile_expr v) ++ leave ++ ret
  | T_Break -> jmp brk
  | T_Continue -> jmp ctn

and compile_var dv = match dv.t_dv_init with
  | None -> nop
  | Some e -> compile_expr e ++ movq !%rax (ind ~ofs:dv.t_dv_id.offset rbp)

and compile_block brk ctn b =
  let compile_decl = function
    | T_Fct _ -> nop
    | T_Var dv -> compile_var dv
    | T_Stmt s -> compile_stmt brk ctn s
  in List.fold_left (fun code d -> code ++ compile_decl d) nop b

let real_fct f =
  pushq !%rbp
  ++ movq !%rsp !%rbp
  ++ subq (imm f.t_df_frame_size) !%rsp
  ++ compile_block undef undef f.t_df_body
  ++ leave
  ++ ret
   
let stack_aligner ext_fct_name =
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  movq (ind ~ofs:begin_offset_arguments rsp) !%rdi ++
  andq (imm (-16)) !%rsp ++
  call ext_fct_name ++
  movq !%rbp !%rsp ++
  popq rbp ++
  ret

let compile_fct f = match f.t_df_id.name with
  | "f_1_malloc" -> stack_aligner "malloc"
  | "f_2_putchar" -> stack_aligner "putchar"
  | _ -> real_fct f

let compile_prog prog funs main_id =
  let tx = ref (globl "main") in
  List.iter (fun f -> tx := !tx ++ label f.t_df_id.name ++ compile_fct f) funs;
  tx := !tx ++ label "main" ++ call main_id ++ movq (imm 0) !%rax ++ ret;
  let p =
    {
      text = !tx;
      data = nop
    }
  in
  p
