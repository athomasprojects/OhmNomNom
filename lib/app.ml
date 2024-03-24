type tab =
  | Home
  | Inspect
  | Logs

let tab_to_string = function
  | Home -> "Home"
  | Inspect -> "Inspect"
  | Logs -> "Logs"
;;

module Input : sig
  type t

  val init : dir:string -> area:string -> t
  val dir : t -> string
  val area : t -> string
  val update_dir : t -> string -> unit
  val update_area : t -> string -> unit
  val print_dir : [< `init | `update ] -> t -> unit
end = struct
  type t =
    { mutable dir : string
    ; mutable area : string
    }

  let init ~dir ~area = { dir; area }
  let dir t = t.dir
  let area t = t.area
  let update_dir t dir = t.dir <- dir
  let update_area t area = t.area <- area

  let print_dir status t =
    match status with
    | `init -> Fmt.pr "@.==== initial dir: '%s'@." (dir t)
    | `update -> Fmt.pr "==== updated dir: '%s'@." (dir t)
  ;;
end

module State = struct
  open Core

  type t =
    { mutable data : Model.t array
    ; mutable user_input : Input.t
    }

  let init () =
    let user_input = Input.init ~dir:"" ~area:"" in
    let data = [||] in
    { data; user_input }
  ;;

  (* let path = Input.dir user_input in *)
  (* let area_value, area_str = *)
  (*   match String.split (Input.area user_input) ~on:' ' with *)
  (*   | [ v; s ] -> Float.of_string v, s *)
  (*   | _ -> assert false *)
  (* in *)
  (* let data = Model.make_models ~path area_value area_str in *)
end

let state = State.init ()

(* let init ~path area_value area_str = *)
(*   let models = Model.make_models ~path area_value area_str in *)
(*   { models } *)
(* ;; *)

let update_entries data_entry area_entry =
  Input.update_dir state.user_input (data_entry#text |> Core.String.strip);
  Input.update_area state.user_input (area_entry#text |> Core.String.strip);
  ()
;;

let start_button_clicked data_entry area_entry () =
  update_entries data_entry area_entry;
  let dir = Input.dir state.user_input in
  let area = Input.area state.user_input in
  let is_dir =
    try Stdlib.Sys.is_directory dir with
    | Sys_error _ ->
      Fmt.pr "%s is not a directory!@." dir;
      false
  in
  if is_dir then Fmt.pr "Run button clicked: you entered: %s@." dir;
  Fmt.pr "Area: %s@." area;
  (* print_dir `update; *)
  flush stdout
;;

let create_ui () =
  let open Core in
  let _ = GMain.init () in
  let window =
    GWindow.window ~title:"OhmNomNom" ~border_width:10 () ~resizable:true
  in
  let _ = window#connect#destroy ~callback:GMain.quit in
  let vbox = GPack.vbox ~spacing:6 ~packing:window#add () in
  let data_hbox =
    GPack.hbox
      ~spacing:6
      ~packing:(vbox#pack ~expand:false ~fill:false ~padding:0)
      ()
  in
  let area_hbox =
    GPack.hbox
      ~spacing:6
      ~packing:(vbox#pack ~expand:false ~fill:false ~padding:0)
      ()
  in
  let start_button =
    GButton.button
      ~label:"Run"
      ~packing:(data_hbox#pack ~expand:false ~fill:false ~padding:0)
      ()
  in
  let entry =
    GEdit.entry
      ~text:"/path/to/data/" (* ~width:1200 *)
      ~packing:(data_hbox#pack ~expand:true ~fill:true ~padding:0)
      ()
  in
  let _ =
    GMisc.label
      ~text:"Area:"
      ~packing:(area_hbox#pack ~expand:false ~fill:false ~padding:3)
      ()
  in
  let area_entry =
    GEdit.entry
      ~text:"0.36 cm^2"
      ~width:250
      ~packing:(area_hbox#pack ~expand:false ~fill:true ~padding:0)
      ()
  in
  (* entry#set_text "path/to/data/"; *)
  let notebook =
    GPack.notebook
      ~scrollable:true
      ~packing:(vbox#pack ~expand:true ~fill:true ~padding:0)
      ()
  in
  List.iter [ Home; Inspect; Logs ] ~f:(fun tab ->
    let tab_str = tab_to_string tab in
    let lbl = GMisc.label ~text:tab_str () in
    let page = GPack.box `HORIZONTAL ~border_width:10 () in
    let _ = notebook#append_page ~tab_label:lbl#coerce page#coerce in
    let () =
      let nrows = 2 in
      let ncols = 4 in
      Fmt.pr "Adding %s page...@." tab_str;
      match tab with
      | Home ->
        let grid =
          GPack.table
            ~columns:ncols
            ~rows:nrows
            ~row_spacings:6
            ~col_spacings:6
            ~homogeneous:true
            ()
        in
        let rows = Util.range_seq 0 (pred nrows) in
        let cols = Util.range_seq 0 (pred ncols) in
        Seq.iter
          (fun i ->
            Seq.iter
              (fun j ->
                let _ =
                  grid#attach
                    ~left:j
                    ~top:i
                    ~expand:`BOTH
                    ~shrink:`BOTH
                    (GMisc.image
                       ~file:
                         "/tmp/ohm_figs/semilog_2114_post_annealing_p360_r2c1_dark.png"
                       ())
                      #coerce
                in
                ())
              cols;
            ())
          rows;
        page#add grid#coerce
        (* page#add (GMisc.label ~text:"This is the home page!" ())#coerce *)
      | Inspect ->
        let file = "assets/admiral.jpg" in
        page#add (GMisc.image ~file ())#coerce
      | Logs -> page#add (GMisc.label ~text:"Logs go here" ())#coerce
    in
    ());
  let _ =
    start_button#connect#clicked ~callback:(fun () ->
      start_button_clicked entry area_entry ())
  in
  window#set_default_size ~width:300 ~height:200;
  window#show ();
  GMain.main ()
;;

let main () = create_ui ()

let run () =
  Input.print_dir `init state.user_input;
  main ()
;;
