let dir = ref ""
let area = ref ""

type tab =
  | Home
  | Inspect
  | Logs

let tab_to_string = function
  | Home -> "Home"
  | Inspect -> "Inspect"
  | Logs -> "Logs"
;;

let print_dir state =
  match state with
  | `init -> Fmt.pr "@.==== initial dir: '%s'@." !dir
  | `update -> Fmt.pr "==== updating dir: '%s'@." !dir
;;

let check_entry entry area_entry =
  let path = entry#text in
  let is_dir =
    try Stdlib.Sys.is_directory path with
    | Sys_error _ ->
      Fmt.pr "%s is not a directory!@." path;
      false
  in
  if is_dir then Fmt.pr "Run button clicked: you entered: %s@." path;
  dir := path;
  area := area_entry#text |> Core.String.strip;
  Fmt.pr "Area: %s@." !area;
  (* print_dir `update; *)
  flush stdout;
  is_dir
;;

let start_button_clicked entry area_entry () =
  let _ = check_entry entry area_entry in
  ()
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
  print_dir `init;
  main ()
;;
