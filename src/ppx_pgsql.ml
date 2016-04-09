open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let hello_mapper _argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_constant (Const_string (str, _)) } ->
        { expr with pexp_desc = Pexp_constant (Const_string ("!" ^ str, None)) }
      | other ->
        default_mapper.expr mapper other
  }

let _ = register "hello" hello_mapper
