open Lwt

let rec lwt_while (a: 'a) (cond: 'a -> bool) (exec: 'a -> 'a Lwt.t): unit Lwt.t =
	if cond a then
		exec a >>=
		fun a' -> lwt_while a' cond exec 
	else
		return ();;

let output_char (oc: Lwt_chan.out_channel) (c: char): unit Lwt.t =
	Lwt_chan.output_string oc (String.make 1 c);;

let output_binary_int (ch: Lwt_chan.out_channel) (i: int): unit Lwt.t =
let output = String.create 4 in
	output.[0] <- char_of_int (i lsr 24 mod 256);
	output.[1] <- char_of_int (i lsr 16 mod 256);
	output.[2] <- char_of_int (i lsr 8 mod 256);
	output.[3] <- char_of_int (i mod 256);
	Lwt_chan.output_string ch output;;

let input_binary_int (ch: Lwt_chan.in_channel): int Lwt.t =
	let result = ref 0 in
	Lwt_chan.input_char ch >>=
	fun een -> result := int_of_char een;
	Lwt_chan.input_char ch >>=
	fun twee -> result := (!result lsl 8) + (int_of_char twee);
 	Lwt_chan.input_char ch >>=
	fun drie -> result := (!result lsl 8) + (int_of_char drie);
 	Lwt_chan.input_char ch >>=
	fun vier -> result := (!result lsl 8) + (int_of_char vier);
	return !result;;
