let string_literal = function
  | Ppxlib.Pconst_string (s, _, _) -> Some s
  | _ -> None
