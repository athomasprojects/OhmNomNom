let tmp_path = "/tmp/ohm_figs"

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

  type sorted_files =
    { mutable all_linear : string
    ; mutable all_semilog : string
    ; mutable by_pitch_linear : string array
    ; mutable by_pitch_semilog : string array
    }

  type t =
    { mutable data : Model.t array
    ; mutable user_input : Input.t
    ; mutable filenames : sorted_files
    }

  let init () =
    let user_input = Input.init ~dir:"" ~area:"" in
    let data = [||] in
    let filenames =
      { all_linear = ""
      ; all_semilog = ""
      ; by_pitch_linear = [||]
      ; by_pitch_semilog = [||]
      }
    in
    { data; user_input; filenames }
  ;;

  let get_homepage_filenames () =
    let open Core in
    let filter_files x spec =
      Array.filter x ~f:(fun f -> String.is_substring ~substring:spec f)
    in
    let linear = "linear" in
    let semilog = "semilog" in
    let files = Stdlib.Sys.readdir "/tmp/ohm_figs" in
    let sorted =
      let all = filter_files files "all" in
      let all_linear =
        let s = filter_files all linear in
        s.(0)
      in
      let all_semilog =
        let s = filter_files all semilog in
        s.(0)
      in
      let by_pitch =
        Array.filter files ~f:(fun f ->
          String.chop_suffix_exn f ~suffix:".png"
          |> String.is_substring ~substring:"p")
      in
      let by_pitch_linear = filter_files by_pitch linear in
      let by_pitch_semilog = filter_files by_pitch semilog in
      { all_linear; all_semilog; by_pitch_linear; by_pitch_semilog }
    in
    sorted
  ;;

  let update_data t =
    let path = Input.dir t.user_input in
    let area_value, area_str =
      match String.split (Input.area t.user_input) ~on:' ' with
      | [ v; s ] -> Float.of_string v, s
      | _ -> assert false
    in
    let models = Model.make_models ~path area_value area_str in
    Model.sort_models models;
    t.data <- models;
    t.filenames <- get_homepage_filenames ()
  ;;
end

let state = State.init ()

let update_entries data_entry area_entry =
  Input.update_dir state.user_input (data_entry#text |> Core.String.strip);
  Input.update_area state.user_input (area_entry#text |> Core.String.strip);
  ()
;;

let make_figs () =
  let models = state.data in
  let norm = true in
  let save = true in
  let scales = [ `Linear; `Semilog ] in
  Core.List.iter scales ~f:(fun scale ->
    let _ = Plot.create_all models scale ~norm ~save in
    let _ = Plot.create_by_category `Pitch models scale ~norm ~save in
    ())
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
  if is_dir
  then (
    let _ = Fmt.pr "Run button clicked: you entered: %s@." dir in
    Fmt.pr "Generating figures...@.";
    State.update_data state;
    make_figs ();
    (* if Stdlib.Sys.readdir *)
    Fmt.pr "Area: %s@." area;
    flush stdout)
  else flush stdout
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
      ~text:"0.012544 cm^2"
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
                let fig = "assets/admiral.jpg" in
                Fmt.pr "FIGURE: %s@." fig;
                let _ =
                  grid#attach
                    ~left:j
                    ~top:i
                    ~expand:`BOTH
                    ~shrink:`BOTH
                    (GMisc.image
                       ~file:
                         (* "/tmp/ohm_figs/semilog_2114_post_annealing_p360_r3c1_dark.png" *)
                         fig
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
