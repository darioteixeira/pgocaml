val lwt_while: 'a -> ('a -> bool) -> ('a -> 'a Lwt.t) -> unit Lwt.t

val output_char: Lwt_chan.out_channel -> char -> unit Lwt.t
val output_binary_int: Lwt_chan.out_channel -> int -> unit Lwt.t
val input_binary_int: Lwt_chan.in_channel -> int Lwt.t
