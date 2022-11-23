open Ast

let test_deriv () =
  let e1 = dummy_expr (Ident "var") in
  let e2 = dummy_expr (Ident "iable") in
  let adder = dummy_expr (Binop (Eq, e1, e2)) in
  Format.printf "%a@." pp_expr adder
