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
  let models = Model.make_models ~path 0.36 "cm^2" in
  Fmt.pr "@.@.==== Sorted Models:@.";
  Model.sort_models models;
  Array.iter models ~f:(fun m -> Fmt.pr "@.%a@." Model.pp m);
  let group = `Row in
  Fmt.pr "@.==== Group By %s:@." (Model.string_of_group group);
  let _ =
    let groups = Model.group_by group models in
    List.iteri groups ~f:(fun idx group ->
      List.iter group ~f:(fun m ->
        let pos = Label.string_of_pos m.lbl.pos in
        Fmt.pr "%d: %s@." (succ idx) pos))
  in
  let _ =
    let m = models.(3) in
    let semi = Plot.create m `Semilog in
    (* let linear = Plot.create m `Linear in *)
    Plot.save_fig semi m `Semilog
    (* Plot.save_fig linear m `Linear *)
    (* Plot.show () *)
  in
  App.run ()
;;
