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

type group =
  | All
  | Single
  | Pitch
[@@deriving show, eq, sexp]

type plot_type =
  [ `Linear of group
  | `Semilog of group
  | `Scatter of group
  ]
[@@deriving show, eq, sexp]

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

let init scale m =
  let Model.{ data; lbl; area; file; path } = m in
  let xs = Data.voltage data in
  let ys =
    let y = Data.current data in
    match scale with
    | `Linear -> y
    | `Semilog ->
      Array.map y ~f:(fun i ->
        if Float.is_negative i
        then Float.log10 @@ Float.abs i
        else Float.log10 i)
  in
  let marker = set_marker_style lbl in
  let linestyle = set_linestyle lbl in
  let label = set_plot_label lbl in
  let title = Label.to_string lbl in
  let x_lbl = Data.string_of_volts (Data.units_of_data `Voltage data) in
  let y_lbl = Data.string_of_amps (Data.units_of_data `Current data) in
  let color = None in
  { marker; label; color; linestyle; xs; ys; x_lbl; y_lbl; title }
;;

module LinearJV = struct
  let () =
    Mpl.set_backend (Other "Gtk3Agg");
    Mpl.style_use "ohm.mplstyle"
  ;;

  let marker_size = 30.

  let plot ax styling =
    let { marker; label; color; linestyle; xs; ys; x_lbl; y_lbl; title } =
      styling
    in
    let labels = [| label |] in
    (* Ax.plot ax ~label ~linestyle ~xs ys; *)
    Ax.scatter ax ~marker ~alpha:0.5 ~s:marker_size (Array.zip_exn xs ys);
    Ax.set_title ax title;
    Ax.legend ax ~labels ~loc:Best ();
    Ax.set_xlabel ax x_lbl;
    Ax.set_ylabel ax y_lbl
  ;;
end

let show () = Mpl.show ()

let create (m : Model.t) scale =
  (* let fg_sz = 4. in *)
  (* let figsize = fg_sz, fg_sz in *)
  let fig = Fig.create () in
  (* ~figsize () in *)
  let ax = Fig.add_subplot fig ~nrows:1 ~ncols:1 ~index:1 in
  let styling = init scale m in
  LinearJV.plot ax styling;
  (* Fig.suptitle fig "YO THIS IS A FIGURE"; *)
  fig
;;

let save_fig fig (m : Model.t) scale =
  let path = "/tmp/ohm_figs/" in
  let _ =
    try Stdlib.Sys.is_directory path with
    | Sys_error _ ->
      Stdlib.Sys.mkdir path 0o755;
      true
  in
  let file =
    let s =
      match scale with
      | `Linear -> "linear_"
      | `Semilog -> "semilog_"
    in
    let f = s ^ Model.replace_file_ext m ~old:"txt" ~ext:"png" in
    path ^ f
  in
  Mpl.savefig file;
  let data = Mpl.plot_data `png in
  Stdio.Out_channel.write_all file ~data
;;
