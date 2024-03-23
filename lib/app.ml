let dir = ref ""
let nrows = 2
let ncols = 4

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

let check_entry entry =
  let path = entry#text in
  let is_dir =
    try Stdlib.Sys.is_directory path with
    | Sys_error _ ->
      Fmt.pr "%s is not a directory!@." path;
      false
  in
  if is_dir then Fmt.pr "Run button clicked: You entered: %s@." path;
  dir := path;
  print_dir `update;
  flush stdout;
  is_dir
;;

let start_button_clicked entry () =
  let _ = check_entry entry in
  ()
;;

let main () =
  let open Core in
  let _ = GMain.init () in
  let window = GWindow.window ~title:"OhmNomNom" ~border_width:10 () in
  let _ = window#connect#destroy ~callback:GMain.quit in
  let vbox = GPack.vbox ~spacing:6 ~packing:window#add () in
  let hbox =
    GPack.hbox
      ~spacing:6
      ~packing:(vbox#pack ~expand:false ~fill:false ~padding:0)
      ()
  in
  let start_button =
    GButton.button
      ~label:"Run"
      ~packing:(hbox#pack ~expand:false ~fill:false ~padding:0)
      ()
  in
  let entry =
    GEdit.entry ~packing:(hbox#pack ~expand:true ~fill:true ~padding:0) ()
  in
  entry#set_text "path/to/data/";
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
                    (GMisc.image
                       ~file:
                         "/tmp/ohm_figs/2114_post_annealing_p360_r3c1_dark.png"
                       (* ~width:2 *)
                       (* ~height:2 *)
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
      | _ -> ()
    in
    ());
  let _ =
    start_button#connect#clicked ~callback:(fun () ->
      start_button_clicked entry ())
  in
  (* window#set_default_width 300; *)
  window#show ();
  GMain.main ()
;;

let run () =
  print_dir `init;
  main ()
;;
