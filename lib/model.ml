type t =
  { data : Data.t
  ; lbl : Label.t
  ; area : Area.t
  ; file : string
  ; path : string
  }
[@@deriving show, eq]

let make ~path ~file area_value area_unit =
  let () = assert (Sys.is_directory path) in
  let () = assert (Sys.file_exists (path ^ "/" ^ file)) in
  let lbl = Label.make file in
  let data = Data.read_data path file in
  let area = Area.init (area_value, area_unit) in
  { data; lbl; area; file; path }
;;
