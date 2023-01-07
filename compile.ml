open Ast
open Typed_ast
open X86_64


let rec compile_lvalue expr =
  match expr.t_edesc with
  | T_Ident { offset = o; v_depth = _ } -> nop, ind ~ofs:o rbp
  | T_Unop (Deref, e) -> compile_expr e, ind rax
  | _ -> failwith "impossible"
and compile_expr expr =
  match expr.t_edesc with
  | T_Const (IntCst i) -> movq (imm i) !%rax
  | T_Const (BoolCst b) -> movq (imm (if b then 1 else 0)) !%rax
  | T_Const Null -> movq (imm 0) !%rax
  | T_Ident { offset = o; v_depth = _ } -> movq (ind ~ofs:o rbp) !%rax
  | T_Call (id, args) ->
    List.fold_left (fun code e -> compile_expr e ++ pushq !%rax ++ code) nop args
    ++ pushq (imm 0) (* TODO : rbp du parent *)
    ++ call id.name
    ++ popn (8 * (1 + List.length args))

  | T_Unop (UPlus, e) -> compile_expr e
  | T_Unop (UMinus, e) -> compile_expr e ++ negq !%rax

  | T_Binop (Plus as op, e1, e2)
  | T_Binop (Minus as op, e1, e2)
  | T_Binop (Mul as op, e1, e2) -> 
    compile_expr e2 ++ movq !%rax !%rbx ++ compile_expr e1 ++
    (match op with Plus -> addq | Minus -> subq | Mul -> imulq | _ -> failwith "impossible") !%rbx !%rax
  | T_Binop (Div as op, e1, e2)
  | T_Binop (Mod as op, e1, e2) ->
    compile_expr e2 ++ movq !%rax !%rbx ++ compile_expr e1 ++ idivq !%rbx ++
    (match op with Mod -> movq !%rdx !%rax | _ -> failwith "impossible")

  | T_Assign (e1, e2) ->
    let c1, addr = compile_lvalue e1 in
    compile_expr e2 ++ c1 ++ movq !%rax addr
  | T_Sizeof _ -> movq (imm 8) !%rax
  | _ -> failwith "aaa"

let compile_var dv =
  (match dv.t_dv_init with
    | None -> nop
    | Some e -> compile_expr e) ++
  
  movq !%rax (ind ~ofs:dv.t_dv_id.offset rbp)
  

let compile_stmt = function
  | T_Expr e -> compile_expr e
  | _ -> failwith "stmt pas gere"
let compile_decl = function
| T_Fct _ -> failwith "fonction imbriquee"
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
  tx := !tx ++ label "main" ++ call main_id ++ ret;
  let p =
    { text = !tx;
      data = nop
    }
  in
  p
