let generate_nonce =
  let init_done = ref false in fun n ->
    if not !init_done then (Random.self_init (); init_done := true);
    let char () =
      let i = Random.int 62 in
      if i < 10 then Char.chr (i+48)
      else if i < 36 then Char.chr (i+55)
      else Char.chr (i+61) in
    String.init n (fun _ -> char ())

let scram_sha_256_mechanism = "SCRAM-SHA-256"

let client_first_message ?(user="") mechanisms =
  if List.mem scram_sha_256_mechanism mechanisms then
    let gs2_header = "n,," in (* no channel binding *)
    let nonce = generate_nonce 20 in
    let s = Format.sprintf "%sn=%s,r=%s" gs2_header user nonce in
    Ok (s, scram_sha_256_mechanism, (nonce, gs2_header))
  else
    (* todo: implement scram-sha-256-plus when ssl is available *)
    Error (Format.sprintf "PGOCaml: None of %s mechanism is implemented" (String.concat ", " mechanisms))

let xor a b =
  if String.length b <> String.length a then invalid_arg "xor: both strings must be of same lengths";
  String.mapi (fun i c -> Char.(chr (code c lxor code (String.get b i)))) a

let sha256 msg = Digestif.SHA256.(to_raw_string @@ digest_string msg)
let hmac ~key msg = Digestif.SHA256.(to_raw_string @@ hmac_string ~key msg)

let hi ~password:key ~salt ~count =
  if count <= 0 then invalid_arg "count must be a positive integer" ;
  let rec aux acc x = function
    | 1 -> x
    | i ->
      let acc = hmac ~key acc in
      aux acc (xor x acc) (i-1) in
  let start = hmac ~key (salt ^ "\000\000\000\001") in
  aux start start count

let proof ~user ~password ~client_nonce ~server_nonce ~salt64 ~gs2_header64 ~count =
  let salt = Base64.decode_exn salt64 in
  let salted_password = hi ~password ~salt ~count in
  let client_key = hmac ~key:salted_password "Client Key" in
  let stored_key = sha256 client_key in
  let auth_message = Format.sprintf "n=%s,r=%s,r=%s,s=%s,i=%d,c=%s,r=%s"
      user client_nonce server_nonce salt64 count gs2_header64 server_nonce in
  let client_signature = hmac ~key:stored_key auth_message in
  let client_proof = xor client_key client_signature in
  let client_proof64 = Base64.encode_exn client_proof in
  let server_key = hmac ~key:salted_password "Server Key" in
  let server_signature = hmac ~key:server_key auth_message in
  let server_signature64 = Base64.encode_exn server_signature in
  client_proof64, server_signature64

let client_final_message ?(user="") ~password ~client_info s =
  let client_nonce, gs2_header = client_info in
  let l = List.filter_map (fun s -> match String.index_opt s '=' with
      | None -> None
      | Some i -> Some (String.sub s 0 i, String.sub s (i+1) (String.length s -i-1))
    ) @@ String.split_on_char ',' s in
  let err = Error "PGOCaml: server SASL authentication message not understood" in
  match List.assoc_opt "r" l, List.assoc_opt "s" l, List.assoc_opt "i" l with
  | Some server_nonce, Some salt64, Some count ->
    if String.starts_with ~prefix:client_nonce server_nonce then
      let count = int_of_string count in
      let gs2_header64 = Base64.encode_exn gs2_header in
      let proof, signature = proof ~user ~password ~client_nonce ~server_nonce ~salt64 ~gs2_header64 ~count in
      let s = Format.sprintf "c=%s,r=%s,p=%s" gs2_header64 server_nonce proof in
      Ok (s, signature)
    else err
  | _ -> err
