open Ast
open Typed_ast


let lvalue = function
| Ident _ -> true
| Unop (Deref, _) -> true
| _ -> false
