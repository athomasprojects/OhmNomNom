module P = Owl_plplot.Plot
module Mat = Owl.Mat

let make_plot (m : Model.t) =
  let x =
    let voltage = m.data.voltage in
    Mat.of_array voltage 1 Core.Array.(length voltage)
  in
  let y =
    let current = m.data.current in
    Mat.of_array current 1 Core.Array.(length current)
  in
  let title = Label.recover_filename m.lbl ^ ".png" in
  let h = P.create title in
  let () =
    P.set_title h (Label.to_string m.lbl);
    P.set_xlabel h "V";
    P.set_ylabel h "A";
    P.set_font_size h 8.;
    P.set_pen_size h 3.;
    P.set_output h title;
    P.plot x y
  in
  P.output h
;;
