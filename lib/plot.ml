open Core
open Matplotlib

type styling =
  { marker : char
  ; label : string
  ; color : Matplotlib.Mpl.Color.t option
  ; linestyle : Matplotlib__Mpl.Linestyle.t
  ; xs : float array
  ; ys : float array
  ; x_lbl : string
  ; y_lbl : string
  ; title : string
  }

let set_marker_style (lbl : Label.t) =
  match lbl.annealed with
  | true -> 'h'
  | false -> 'o'
;;

let set_linestyle (lbl : Label.t) =
  Matplotlib__Mpl.Linestyle.(
    match lbl.illum with
    | true -> Solid
    | false -> Other "--")
;;

let set_plot_label (lbl : Label.t) =
  String.capitalize (Label.string_of_pitch lbl.pitch)
  ^ " - "
  ^ Label.string_of_pos lbl.pos
;;

let init scale norm m =
  let Model.{ data; lbl; area; _ } = m in
  let xs = Data.voltage data in
  let ys =
    let y =
      if norm then Data.current_density area data else Data.current data
    in
    match scale with
    | `Linear -> y
    | `Semilog ->
      Array.map y ~f:(fun i -> if Float.is_negative i then Float.abs i else i)
  in
  let marker = set_marker_style lbl in
  let linestyle = set_linestyle lbl in
  let label = set_plot_label lbl in
  let title = "Sample " ^ Int.to_string lbl.id in
  let x_lbl = Data.string_of_volts (Data.units_of_data `Voltage data) in
  let y_lbl =
    let s = Data.string_of_amps (Data.units_of_data `Current data) in
    if norm then s ^ "/" ^ Area.string_of_unit area else s
  in
  let color = None in
  { marker; label; color; linestyle; xs; ys; x_lbl; y_lbl; title }
;;

module ModelPlotter = struct
  let () =
    Mpl.set_backend (Other "Gtk3Agg");
    Mpl.style_use "ohm.mplstyle"
  ;;

  (* let marker_size = 30. *)
  let plot (ax : Matplotlib__.Fig_ax.Ax.t) styling scale =
    let { marker; label; xs; ys; x_lbl; y_lbl; title; _ } = styling in
    let _ =
      match scale with
      | `Linear -> Pyplot.plot ~label ~linestyle:Dotted ~xs ys
      | `Semilog -> Pyplot.semilogy ~label ~linestyle:Dotted ~xs ys
    in
    Pyplot.xlabel x_lbl;
    Pyplot.ylabel y_lbl;
    Pyplot.title title
  ;;
end

let show () = Mpl.show ()

let all_traces_filename scale (m : Model.t) =
  let s =
    match scale with
    | `Linear -> "linear"
    | `Semilog -> "semilog"
  in
  String.concat ~sep:"_" [ "all"; Int.to_string m.lbl.id; s ] ^ ".png"
  |> String.lowercase
;;

let group_filename group scale (m : Model.t) =
  let g =
    match group with
    | `Row -> "row" ^ (fst m.lbl.pos |> Int.to_string)
    | `Col -> "col" ^ (snd m.lbl.pos |> Int.to_string)
    | `Pitch -> "p" ^ (m.lbl.pitch |> Int.to_string)
  in
  let s =
    match scale with
    | `Linear -> "linear"
    | `Semilog -> "semilog"
  in
  String.concat ~sep:"_" [ Int.to_string m.lbl.id; g; s ] ^ ".png"
  |> String.lowercase
;;

let filename scale (m : Model.t) =
  let s =
    match scale with
    | `Linear -> "linear_"
    | `Semilog -> "semilog_"
  in
  s ^ Model.replace_file_ext m ~old:"txt" ~ext:"png"
;;

let save_fig file (m : Model.t) =
  let path = "/tmp/ohm_figs/" in
  let _ =
    try Stdlib.Sys.is_directory path with
    | Sys_error _ ->
      Stdlib.Sys.mkdir path 0o755;
      true
  in
  Mpl.savefig (path ^ file);
  let data = Mpl.plot_data `png in
  Stdio.Out_channel.write_all file ~data
;;

let create scale ~norm m =
  let fig = Fig.create () in
  let ax = Fig.add_subplot fig ~nrows:1 ~ncols:1 ~index:1 in
  let styling = init scale norm m in
  ModelPlotter.plot ax styling scale;
  Pyplot.legend ~labels:[| styling.label |] ~loc:Best ();
  fig
;;

let plot_all_traces models scale ~norm ~save =
  let fig = Fig.create () in
  let ax = Fig.add_subplot fig ~nrows:1 ~ncols:1 ~index:1 in
  let labels =
    Array.map models ~f:(fun m ->
      let styling = init scale norm m in
      ModelPlotter.plot ax styling scale;
      styling.label)
  in
  Pyplot.legend ~labels ~loc:Best ();
  fig
;;

let plot_by_category category models scale ~norm ~save =
  let models' = Model.group_by category models in
  let figs_files =
    List.map models' ~f:(fun group ->
      let fig = Fig.create () in
      let ax = Fig.add_subplot fig ~nrows:1 ~ncols:1 ~index:1 in
      let labels =
        List.map group ~f:(fun m ->
          let styling = init scale norm m in
          ModelPlotter.plot ax styling scale;
          styling.label)
        |> Array.of_list
      in
      Pyplot.legend ~labels ~loc:Best ();
      let m' = List.hd_exn group in
      let file = group_filename category scale m' in
      if save then save_fig file m';
      fig, file)
  in
  figs_files
;;

let create_all models scale ~norm ~save =
  let _ = plot_all_traces models scale ~norm ~save in
  let file = all_traces_filename scale models.(0) in
  if save then save_fig file models.(0)
;;

let create_by_category category models scale ~norm ~save =
  let _ = plot_by_category category models scale ~norm ~save in
  ()
;;
