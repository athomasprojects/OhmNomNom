let check_entry entry =
  let path = entry#text in
  let is_dir =
    try Stdlib.Sys.is_directory path with
    | Sys_error _ ->
      Fmt.pr "%s is not a directory!@." path;
      false
  in
  if is_dir then Fmt.pr "Run button clicked: You entered: %s@." path;
  flush stdout;
  is_dir
;;

let start_button_clicked entry () =
  let _ = check_entry entry in
  ()
;;

let main () =
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
  let tabs = [ "Home"; "Inspect"; "Log" ] in
  List.iter
    (fun tab ->
      let lbl = GMisc.label ~text:tab () in
      let page = GPack.box `HORIZONTAL ~border_width:10 () in
      let _ = notebook#append_page ~tab_label:lbl#coerce page#coerce in
      let () =
        Fmt.pr "Adding %s page...@." tab;
        match tab with
        | "Home" ->
          page#add (GMisc.label ~text:"This is the home page!" ())#coerce
        | "Inspect" ->
          let file = "assets/admiral.jpg" in
          page#add (GMisc.image ~file ())#coerce
        | "Log" -> page#add (GMisc.label ~text:"Logs go here" ())#coerce
        | _ -> ()
      in
      ())
    tabs;
  let _ =
    start_button#connect#clicked ~callback:(fun () ->
      start_button_clicked entry ())
  in
  window#show ();
  GMain.main ()
;;

let run () = main ()
