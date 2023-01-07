open Ast
open Typed_ast
open X86_64


let rec compile_expr expr =
  match expr.t_edesc with
  | T_Const (IntCst i) -> movq (imm i) !%rax
  | T_Const (BoolCst b) -> movq (imm (if b then 1 else 0)) !%rax
  | T_Const Null -> movq (imm 0) !%rax
  | T_Ident { offset = o; v_depth = _ } -> movq (ind ~ofs:o rsp) !%rax
  | T_Call (id, args) ->
    List.fold_left (fun code e -> (compile_expr e) ++ code) nop args
    ++ call id.name
    ++ popn (List.length args)
  | _ -> failwith "aaa"

let stack_aligner ext_fct_name =
  pushq !%rbp ++
  movq !%rsp !%rbp ++
  andq (imm (-16)) !%rsp ++
  call ext_fct_name ++
  movq !%rbp !%rsp ++
  popq rbp ++
  ret

let putchar_wrapper = nop
let compile_fct f = match f.t_df_id.name with
  | "f_1_malloc" -> stack_aligner "malloc"
  | "f_2_putchar" -> stack_aligner "putchar"
  | _ -> ret

let compile_prog prog funs main_id =
  let tx = ref (globl main_id) in
  List.iter (fun f -> tx := !tx ++ label f.t_df_id.name ++ compile_fct f) funs;
  let p =
    { text = !tx;
      data = nop
    }
  in
  p
