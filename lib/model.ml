open Core
module Sys = Stdlib.Sys

type t =
  { data : Data.t
  ; lbl : Label.t
  ; area : Area.t
  ; file : string
  ; path : string
  }
[@@deriving show, eq]

let compare_by_pitch m1 m2 =
  let pos1 = m1.lbl.pos in
  let pos2 = m2.lbl.pos in
  let rows = fst pos1, fst pos2 in
  let cols = snd pos1, snd pos2 in
  let cmp_rows = Int.compare (fst rows) (snd rows) in
  let cmp_cols = Int.compare (fst cols) (snd cols) in
  if cmp_rows <> 0
  then cmp_rows
  else if cmp_cols <> 0
  then cmp_cols
  else failwith "Invalid pad position"
;;

let make_exn ~path ~file area_value area_unit =
  let () = assert (Sys.is_directory path) in
  let () = assert (Sys.file_exists (path ^ file)) in
  let lbl = Label.make file in
  let data = Data.read_data path file in
  let area = Area.init (area_value, area_unit) in
  { data; lbl; area; file; path }
;;

let make ~path ~file area_value area_unit =
  let make_label f =
    let lbl = Label.make f in
    let data = Data.read_data path f in
    let area = Area.init (area_value, area_unit) in
    { data; lbl; area; file; path }
  in
  let valid_file =
    let cond = Sys.is_directory path && Sys.file_exists (path ^ file) in
    Option.some_if cond file
  in
  Option.bind valid_file ~f:(fun _ -> Some (make_label file))
;;

let make_models ~path area_value area_unit =
  let files = Sys.readdir path in
  Array.iter files ~f:(fun file -> Fmt.pr "Found: %s@." file);
  Array.filter_map files ~f:(fun file ->
    if String.is_suffix file ~suffix:".txt"
    then make ~path ~file area_value area_unit
    else None)
;;

let make_models_exn ~path area_value area_unit =
  let files = Sys.readdir path in
  Array.map files ~f:(fun file -> make_exn ~path ~file area_value area_unit)
;;

let sort_models models = Array.sort models ~compare:compare_by_pitch

let string_of_group = function
  | `Row -> "Row"
  | `Col -> "Column"
  | `Pitch -> "Pitch"
;;

let group_by group models =
  if not (Array.is_sorted models ~compare:compare_by_pitch)
  then sort_models models
  else ();
  let max_pos =
    Array.fold models ~init:(0, 0) ~f:(fun acc m ->
      let row = Int.max (fst m.lbl.pos) (fst acc) in
      let col = Int.max (snd m.lbl.pos) (snd acc) in
      row, col)
  in
  let groups =
    match group with
    | `Row ->
      let chunks = fst max_pos in
      Array.create ~len:chunks []
      |> Array.mapi ~f:(fun idx _ ->
        Array.filter models ~f:(fun m ->
          let row = fst m.lbl.pos in
          row = succ idx)
        |> List.of_array)
    | `Col ->
      let chunks = snd max_pos in
      Array.create ~len:chunks []
      |> Array.mapi ~f:(fun idx _ ->
        Array.filter models ~f:(fun m ->
          let col = snd m.lbl.pos in
          col = succ idx)
        |> List.of_array)
    | `Pitch ->
      let pitches =
        List.of_array models
        |> List.map ~f:(fun m -> m.lbl.pitch)
        |> List.dedup_and_sort ~compare:(fun p1 p2 -> compare p1 p2)
        |> Array.of_list
      in
      Array.create ~len:(Array.length pitches) []
      |> Array.mapi ~f:(fun idx _ ->
        Array.filter models ~f:(fun m -> pitches.(idx) = m.lbl.pitch)
        |> List.of_array)
  in
  List.of_array groups
;;

let set_file_ext m ~suffix =
  let { file } = m in
  if String.is_suffix file ~suffix then file else file ^ "." ^ suffix
;;

let strip_file_ext m ~suffix =
  String.chop_suffix_if_exists m.file ~suffix:("." ^ suffix)
;;

let replace_file_ext m ~old ~ext =
  let prefix = strip_file_ext m ~suffix:old in
  prefix ^ "." ^ ext
;;
