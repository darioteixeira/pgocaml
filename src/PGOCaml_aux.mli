module String :
  sig
    include module type of String

    val starts_with : string -> string -> bool
    val join : string -> string list -> string
    val implode : char list -> string
    val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
    val init: int -> (int -> char) -> string
  end

module Option :
  sig
    val default : 'a -> 'a option -> 'a
    val get : 'a option -> 'a
    val map : ('a -> 'b) -> 'a option -> 'b option
  end


module List :
  sig
    include module type of List

    val iteri : (int -> 'a -> unit) -> 'a list -> unit
    val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
  end
