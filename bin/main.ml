module Data = Ohmnom.Data
module Area = Ohmnom.Area

let () =
  let path = "data/" in
  let fname = "2114_post_annealing_p360_r2c1_dark.txt" in
  (* let fname = "test.txt" in *)
  let data = Data.read_data path fname in
  let area = Area.init (1.e-8, "cm^2") in
  Fmt.pr "@.==== Data:";
  let _ = Fmt.pr "@.%a@." Data.pp data in
  Fmt.pr "@.==== Area:";
  let _ = Fmt.pr "@.%a@." Area.pp area in
  let _ = Fmt.pr "`Area.to_string`: %s@." (Area.to_string area) in
  ()
;;
