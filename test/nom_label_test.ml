open Core
module Label = Ohmnom.Label

module Test = struct
  let label_testable = Alcotest.testable Label.pp Label.equal

  let test_label_make () =
    (* TODO: Move label test cases into separate tests for better debugging. *)
    let lbls =
      List.map
        [ "2114_post_annealing_p360_r2c3_light.txt"
        ; "1068_before_annealing_p257_r1c0_dark.txt"
        ; "452_after_annealing_p123456789_r3c3_dark.txt"
        ; "3_pre_annealing_p69420_r5c20_light.txt"
        ]
        ~f:Label.make
    in
    let expected : Label.t list =
      [ { id = 2114; annealed = true; pitch = 360; pos = 2, 3; illum = true }
      ; { id = 1068; annealed = false; pitch = 257; pos = 1, 0; illum = false }
      ; { id = 452
        ; annealed = true
        ; pitch = 123456789
        ; pos = 3, 3
        ; illum = false
        }
      ; { id = 3; annealed = false; pitch = 69420; pos = 5, 20; illum = true }
      ]
    in
    List.iter2_exn lbls expected ~f:(fun lbl expected ->
      Alcotest.(check label_testable) "same Label.t" expected lbl)
  ;;

  let test_label_recover_filename () =
    let expected = "2114_post_annealing_p360_r2c3_light" in
    let lbl =
      Label.make "2114_post_annealing_p360_r2c3_light.txt" |> Label.to_string
    in
    Alcotest.(check string) "same string" expected lbl
  ;;
end

let () =
  Alcotest.run
    "Testing `Label`"
    [ ( "parse_label"
      , [ Alcotest.test_case
            "Parse file name to `Label.t`"
            `Quick
            Test.test_label_make
        ] )
    ; ( "to_string"
      , [ Alcotest.test_case
            "Recover original filename"
            `Quick
            Test.test_label_recover_filename
        ] )
    ]
;;
