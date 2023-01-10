open Ast
open Typed_ast
open X86_64


let rec compile_lvalue expr =
  (* Pour une lvalue, renvoie l'adresse dans laquelle est stockée la valeur, au lieu de la valeur elle même *)
  match expr.t_edesc with
  | T_Ident { offset = o; v_depth = _ } -> movq !%rbp !%rax ++ addq (imm o) !%rax
  | T_Unop (Deref, e) -> compile_expr e
  | _ -> failwith "impossible"
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
  | T_Unop (Incr true as op, e) | T_Unop (Decr true as op, e) ->
    compile_lvalue e
    ++ (match op with Incr _ -> incq | Decr _ -> decq | _ -> failwith "impossible") (ind rax)
    ++ movq (ind rax) !%rax
  | T_Unop (Incr false as op, e) | T_Unop (Decr false as op, e) ->
    compile_lvalue e
    ++ movq !%rax !%rbx
    ++ movq (ind rbx) !%rax
    ++ (match op with Incr _ -> incq | Decr _ -> decq | _ -> failwith "impossible") (ind rbx)

  | T_Binop (Plus as op, e1, e2)
  | T_Binop (Minus as op, e1, e2)
  | T_Binop (Mul as op, e1, e2) -> 
    compile_expr e2
    ++ pushq !%rax (* On sauvegarde le calcul du 2e terme *)
    ++ compile_expr e1
    ++ popq rbx
    ++ (match op with Plus -> addq | Minus -> subq | Mul -> imulq | _ -> failwith "impossible") !%rbx !%rax
  | T_Binop (Div as op, e1, e2)
  | T_Binop (Mod as op, e1, e2) ->
    compile_expr e2
    ++ pushq !%rax (* On sauvegarde le calcul du 2e terme *)
    ++ compile_expr e1
    ++ popq rbx
    ++ movq (imm 0) !%rdx (* Il faut mettre à 0 les 64 derniers bits du dividende *)
    ++ idivq !%rbx
    ++ (match op with Div -> nop | Mod -> movq !%rdx !%rax | _ -> failwith "impossible")

  | T_Assign (e1, e2) ->
    compile_expr e2
    ++ pushq !%rax (* On sauvegarde le calcul du 2e terme *)
    ++ compile_lvalue e1
    ++ popq rbx
    ++ movq !%rbx (ind rax)
    ++ movq !%rbx !%rax (* a = b renvoie la valeur de a après assignation, c'est à dire la valeur de b *)
  | T_Sizeof _ -> movq (imm 8) !%rax
  | _ -> failwith "aaa"

let compile_var dv =
  (match dv.t_dv_init with
    | None -> nop
    | Some e -> compile_expr e)
  ++ movq !%rax (ind ~ofs:dv.t_dv_id.offset rbp)
  

let compile_stmt = function
  | T_Expr e -> compile_expr e
  | _ -> failwith "stmt pas gere"
let compile_decl = function
| T_Fct _ -> nop
| T_Var dv -> compile_var dv
| T_Stmt s -> compile_stmt s
let real_fct f =
  let code = ref (pushq !%rbp ++ movq !%rsp !%rbp ++ subq (imm f.t_df_frame_size) !%rsp) in
  List.iter (fun d -> code := !code ++ compile_decl d) f.t_df_body;
  !code ++ leave ++ ret
   
let stack_aligner ext_fct_name =
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  movq (ind ~ofs:begin_offset_arguments rsp) !%rdi ++
  andq (imm (-16)) !%rsp ++
  call ext_fct_name ++
  movq !%rbp !%rsp ++
  popq rbp ++
  ret

let putchar_wrapper = nop
let compile_fct f = match f.t_df_id.name with
  | "f_1_malloc" -> stack_aligner "malloc"
  | "f_2_putchar" -> stack_aligner "putchar"
  | _ -> real_fct f

let compile_prog prog funs main_id =
  let tx = ref (globl "main") in
  List.iter (fun f -> tx := !tx ++ label f.t_df_id.name ++ compile_fct f) funs;
  tx := !tx ++ label "main" ++ call main_id ++ movq (imm 0) !%rax ++ ret;
  let p =
    { text = !tx;
      data = nop
    }
  in
  p
