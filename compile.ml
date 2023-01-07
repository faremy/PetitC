open Ast
open Typed_ast
open X86_64


let rec compile_expr expr =
  match expr.t_edesc with
  | T_Const (IntCst i) -> movq (imm i) !%rax
  | T_Const (BoolCst b) -> movq (imm (if b then 1 else 0)) !%rax
  | T_Const Null -> movq (imm 0) !%rax
  | _ -> failwith "aaa"
