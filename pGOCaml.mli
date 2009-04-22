(* PG'OCaml is a set of OCaml bindings for the PostgreSQL database.
 * $Id: pGOCaml.mli,v 1.14 2007-10-14 14:52:27 rich Exp $
 *)

include PGOCaml_generic.PGOCAML_GENERIC
  with type 'a monad = 'a
