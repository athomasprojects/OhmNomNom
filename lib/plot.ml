open Core
open Matplotlib

type theme =
  { marker : char
  ; label : string
  ; color : Matplotlib.Mpl.Color.t
  ; linewidth : float
  ; linestyle : Matplotlib.Mpl.Linestyle.t
  ; style : string
  }

type group =
  | All
  | Single
  | Pitch

type plots =
  | Linear of group
  | Semilog of group
  | Scatter of group

let () =
  (* Mpl.set_backend Agg; *)
  Mpl.style_use "plotting.mplstyle"
;;

let set_marker_style (lbl : Label.t) =
  match lbl.annealed with
  | true -> 'o'
  | false -> 's'
;;

let set_linestyle (lbl : Label.t) =
  Matplotlib__Mpl.Linestyle.(
    match lbl.illum with
    | true -> Solid
    | false -> Other "--")
;;

let set_plot_label (lbl : Label.t) =
  String.concat
    ~sep:" - "
    [ Label.string_of_pitch lbl.pitch; Label.string_of_pos lbl.pos ]
;;

module LinearJV = struct
  let plot ax m =
    let Model.{ data; lbl; area; file; path } = m in
    let _size = 50 in
    let xs = data.voltage in
    let ys = data.current in
    (* Set plot style *)
    let _marker = set_marker_style lbl in
    let linestyle = set_linestyle lbl in
    let label = set_plot_label lbl in
    let labels = label |> Array.create ~len:1 in
    let title = Label.to_string lbl in
    let x_lbl = Data.string_of_volts data.v_units in
    let y_lbl = Data.string_of_amps data.i_units in
    Ax.set_title ax title;
    Ax.set_xlabel ax x_lbl;
    Ax.set_ylabel ax y_lbl;
    Ax.legend ax ~labels ();
    Ax.plot ax ~label ~linestyle ~xs ys
  ;;

  let semilog ax m =
    let Model.{ data; lbl; area; file; path } = m in
    let _size = 50 in
    let xs = Array.map data.voltage ~f:Float.log10 in
    let ys = data.current in
    (* Set plot style *)
    let _marker = set_marker_style lbl in
    let linestyle = set_linestyle lbl in
    let label = set_plot_label lbl in
    let labels = label |> Array.create ~len:1 in
    let title = Label.to_string lbl in
    let x_lbl = Data.string_of_volts data.v_units in
    let y_lbl = Data.string_of_amps data.i_units in
    Ax.set_title ax title;
    Ax.set_xlabel ax x_lbl;
    Ax.set_ylabel ax y_lbl;
    Ax.legend ax ~labels ();
    Ax.plot ax ~label ~linestyle ~xs ys
  ;;
end

let create (m : Model.t) =
  let figsize = 3.5, 3.5 in
  let fig = Fig.create ~figsize () in
  let ax = Fig.add_subplot fig ~nrows:1 ~ncols:1 ~index:1 in
  (* LinearJV.plot ax m; *)
  LinearJV.semilog ax m;
  (* Fig.suptitle fig "YO THIS IS A FIGURE"; *)
  Mpl.show ()
;;
