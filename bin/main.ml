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
  (* let idx = 6 in *)
  (* let _ = *)
  (*   let m = models.(idx) in *)
  (*   let scale = `Semilog in *)
  (*   let _semi = Plot.create scale ~norm:true m in *)
  (*   let file = Plot.filename scale m in *)
  (*   Plot.save_fig file m *)
  (* in *)
  (* let _ = *)
  (*   let m = models.(idx) in *)
  (*   let scale = `Linear in *)
  (*   let _linear = Plot.create scale ~norm:true m in *)
  (*   let file = Plot.filename scale m in *)
  (*   Plot.save_fig file m *)
  (*   (* Plot.show () *) *)
  (* in *)
  (* let _ = Plot.create_all models `Semilog ~norm:true ~save:true in *)
  (* let _ = Plot.create_by_category `Row models `Semilog ~norm:true ~save:true in *)
  (* let _ = Plot.create_by_category `Row models `Linear ~norm:true ~save:true in *)
  (* let _ = Plot.create_by_category `Col models `Semilog ~norm:true ~save:true in *)
  (* let _ = Plot.create_by_category `Col models `Linear ~norm:true ~save:true in *)
  (* let _ = *)
  (*   Plot.create_by_category `Pitch models `Semilog ~norm:true ~save:true *)
  (* in *)
  (* let _ = Plot.create_by_category `Pitch models `Linear ~norm:true ~save:true in *)
  App.run ()
;;
