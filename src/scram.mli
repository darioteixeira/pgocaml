val client_first_message: ?user:string -> string list -> (string * string * (string * string), string) result
val client_final_message: ?user: string -> password:string -> client_info:(string * string) -> string -> (string * string, string) result
