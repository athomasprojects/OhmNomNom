open Core
module Label = Ohmnomnom.Label

module Test = struct
  let label_testable = Alcotest.testable Label.pp Label.equal

  let test_parse_label () =
    let lbls =
      List.map
        [ "2114_post_annealing_P360_r2c3_light.txt"
        ; "1068_before_annealing_p257_r1c0_dark.txt"
        ; "452_after_annealing_p123456789_r3c3_dark.txt"
        ; "3_pre_annealing_P69420_r5c20_light.txt"
        ]
        ~f:Label.parse_label
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
end

let () =
  Alcotest.run
    "Ohmnom Label Test Suite"
    [ ( "init-lexer"
      , [ Alcotest.test_case "Parse file name" `Quick Test.test_parse_label ] )
    ]
;;
