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

let interval_of_string =
  let printer = PGOCaml.string_of_interval in
  let module Period = CalendarLib.Calendar.Period in
  let add' = List.fold_left Period.add Period.empty in
  [ "5 years", Period.year 5
  ; "12 day", Period.day 12
  ; "06:00", Period.hour 6
  ; "00:10", Period.minute 10
  ; "5 years 3 mons", Period.(add (year 5) (month 3))
  ; "12 year 00:12:03", Period.(add' [year 12; minute 12; second 3])
  ; "77 day 12:12", Period.(add' [day 77; hour 12; minute 12])
  ]
  |> List.map (fun (s, p) ->
    s >:: fun () ->
      assert_equal ~printer p (PGOCaml.interval_of_string s)
  )

let () =
  ("test res" >:::
   [ "inet_of_string" >::: inet_of_string
   ; "point_of_string" >::: point_of_string
   ; "interval_of_string" >::: interval_of_string
   ]
  )
  |> run_test_tt_main
  |> ignore
