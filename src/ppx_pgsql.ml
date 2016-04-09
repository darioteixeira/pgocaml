open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let pgocaml_mapper _argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "pgsql"; loc }, pstr); } ->
        Exp.assert_ ~loc {
          pexp_desc = (Pexp_construct ({ txt = Lident "false"; loc }, None));
          pexp_loc = loc;
          pexp_attributes = [];
        }
      | other ->
        default_mapper.expr mapper other
  }

let _ = register "pgocaml" pgocaml_mapper
