open OUnit

let sprintf = Printf.sprintf

let inet_of_string =
  let printer (inet, addr) =
    sprintf "%s/%d" (Unix.string_of_inet_addr inet) addr in
  [ "123.100.1.1/16", ("123.100.1.1", 16)
  ; "127.0.0.1", ("127.0.0.1", 32)
  ; "1.1.1.1/", ("1.1.1.1", 32)
  ; "fe80::0202:b3ff:fe1e:8329", ("fe80::0202:b3ff:fe1e:8329", 128) ]
  |> List.map (fun (s, (str, mask)) ->
    let ip = Unix.inet_addr_of_string str in
    s >:: fun () ->
      assert_equal ~printer (ip, mask) (PGOCaml.inet_of_string s)
  )

let point_of_string =
  let printer (f1, f2) = sprintf "(%f, %f)" f1 f2 in
  let tests =
    [ "(0,0)", (0.0, 0.0)
    ; "(+1,-2)", (1.0, -2.0)
    ; "(1.0, 2.0)", (1.0, 2.0)
    ; "(   1,1.0)", (1.0, 1.0)
    ; "(1.0, 1.0 )", (1.0, 1.0)
    ; "(+Infinity, -Infinity)", (infinity, neg_infinity)
    ; "(12.0, Infinity  )", (12.0, infinity)
    ]
    |> List.map (fun (s, pt) ->
      s >:: fun () ->
        assert_equal ~printer pt (PGOCaml.point_of_string s)
    ) in
  ("nan" >:: fun () ->
     ignore (PGOCaml.point_of_string "(nan ,  NaN )"))::tests

let () =
  ("test res" >:::
   [ "inet_of_string" >::: inet_of_string
   ; "point_of_string" >::: point_of_string
   ]
  )
  |> run_test_tt_main
  |> ignore
