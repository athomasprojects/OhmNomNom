open Core
module Label = Ohmnom.Label
module Model = Ohmnom.Model
module Plot = Ohmnom.Plot
module App = Ohmnom.App

let () =
  let path = "test_data/" in
  (* let file = "2114_post_annealing_p360_r2c2_dark.txt" in *)
  (* let model = Model.make ~path ~file 0.36 "cm^2" |> Option.value_exn in *)
  (* Fmt.pr "@.==== Model:@."; *)
  (* Fmt.pr "@.%a@." Model.pp model; *)
  let models = Model.make_models ~path 0.012544 "cm^2" in
  Model.sort_models models;
  Model.print_sorted_model_array models;
  Model.print_grouping `Pitch models;
  let idx = 6 in
  let _ =
    let m = models.(idx) in
    let _semi = Plot.create `Semilog ~norm:true m in
    Plot.save_fig `Semilog m
  in
  let _ =
    let m = models.(idx) in
    let _linear = Plot.create `Linear ~norm:true m in
    Plot.save_fig `Linear m
    (* Plot.show () *)
  in
  let _ = Plot.create_all models `Semilog ~norm:true ~save:true in
  let _ = Plot.create_by_category `Row models `Semilog ~norm:true ~save:true in
  App.run ()
;;
