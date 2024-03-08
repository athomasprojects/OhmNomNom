open Core
open Matplotlib

module Plot = struct
  type scale =
    | Linear
    | Semilog

  type plot_group =
    | All of scale
    | Single of scale
    | Pitch of scale

  type t =
    { marker : char
    ; label : string
    ; color : Matplotlib.Mpl.Color.t
    ; linewidth : float
    ; linestyle : Matplotlib.Mpl.Linestyle.t
    ; style : string
    ; data : float array * float array
    }

  let style = "plotting.mplstyle"

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

  (* let theme_of_model m = *)
  (*   let Model.{ data; lbl; area; file; path } = m in *)
  (*   let marker = set_marker_style lbl in *)
  (*   let linestyle = set_linestyle lbl in *)
  (*   let label, labels = *)
  (*     let l = plot_label lbl *)
  (*     in *)
  (**)
end
