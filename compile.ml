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

let compile_prog prog funs =
  let p =
    { text =
        globl "main" ++ label "main" ++
        ret;
      data = nop
    }
  in
  p
