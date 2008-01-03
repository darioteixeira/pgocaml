open Lwt

let rec lwt_while (var: 'a) (cond: 'a -> bool) (f: 'a -> 'a Lwt.t): unit Lwt.t =
	if (cond var) then (f var >>= fun var' -> lwt_while var' cond f)
	else return ();;

let rec mapi_aux i f l =
	match l with
	| [] -> return []
	| v::r ->
		let t = f i v in
		let rt = mapi_aux (i+1) f r in
		t >>= (fun v' ->
		rt >>= (fun l' ->
		return (v'::l')));;

let mapi (f: int -> 'a -> 'b Lwt.t) (l: 'a list): 'b list Lwt.t =
	mapi_aux 0 f l;;

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
