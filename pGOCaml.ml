(* PG'OCaml is a set of OCaml bindings for the PostgreSQL database.
 * $Id: pGOCaml.ml,v 1.24 2007-10-14 14:52:27 rich Exp $
 *)

module M = PGOCaml_generic.Make (struct
  type 'a t = 'a
  let return x = x
  let (>>=) v f =  f v
  let fail = raise

  type in_channel = Pervasives.in_channel
  type out_channel = Pervasives.out_channel
  let open_connection = Unix.open_connection
  let output_char = output_char
  let output_binary_int = output_binary_int
  let output_string = output_string
  let flush = flush
  let input_char = input_char
  let input_binary_int = input_binary_int
  let really_input = really_input
  let close_in = close_in
end)

include M
