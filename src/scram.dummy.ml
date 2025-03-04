let scram_sha_256_mechanism = "SCRAM-SHA-256"

let client_first_message ?user:_ mechanisms =
  if List.mem scram_sha_256_mechanism mechanisms then
    Error "PGOCaml: SCRAM-SHA-256 authentication requires base64 and digestif optional dependencies of pgocaml"
  else
    Error (Format.sprintf "PGOCaml: None of %s mechanism is implemented" (String.concat ", " mechanisms))

let client_final_message ?user:_ ~password:_ ~client_info:_ _msg =
  Error "PGOCaml: SASL authentication not implemented"
