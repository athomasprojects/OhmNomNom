module Model = Ohmnom.Model
module Plot = Ohmnom.Plot

let () =
  let path = "data/" in
  let file = "2114_post_annealing_p360_r2c1_dark.txt" in
  let model = Model.make ~path ~file 0.36 "cm^2" in
  Fmt.pr "@.==== Model:@.";
  Fmt.pr "@.%a@." Model.pp model;
  Plot.create model;
  ()
;;
