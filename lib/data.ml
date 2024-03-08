open Core

type t =
  { mutable current : float array
  ; mutable voltage : float array
  ; v_units : Prefix.t
  ; i_units : Prefix.t
  }
[@@deriving show, eq]

let voltage t = t.voltage
let current t = t.current

let units_of_data qty t =
  match qty with
  | `Voltage -> t.v_units
  | `Current -> t.i_units
;;

let string_of_volts prefix = Prefix.string_of_prefix prefix ^ "V"
let string_of_amps prefix = Prefix.string_of_prefix prefix ^ "A"

let read_units str =
  let voltage_prefix =
    let open Parser.A in
    char '('
    *> take_till (fun ch ->
      match ch with
      | 'V' -> true
      | _ -> false)
    >>| fun t ->
    match Prefix.prefix_of_string_opt t with
    | Some p -> p
    | None -> failwith "Invalid voltage unit prefix."
  in
  let current_prefix =
    let open Parser.A in
    char '('
    *> take_till (fun ch ->
      match ch with
      | 'A' -> true
      | _ -> false)
    >>| fun t ->
    match Prefix.prefix_of_string_opt t with
    | Some p -> p
    | None -> failwith "Invalid current unit prefix."
  in
  let open Parser.A in
  let parser =
    let open Parser.A in
    let* _ = take_till (Char.equal '(') in
    let* vpref = voltage_prefix in
    let* _ = take_till (Char.equal '(') in
    let* ipref = current_prefix in
    return (vpref, ipref)
  in
  parse_string ~consume:Prefix parser str |> Result.ok_or_failwith
;;

let read_measurements str =
  let open Parser.A in
  let parser =
    let* voltage = float <* whitespace in
    let* current = float in
    return (voltage, current)
  in
  parse_string ~consume:Prefix parser str |> Result.ok_or_failwith
;;

let read_data path filename =
  let () = assert (Stdlib.Sys.is_directory path) in
  let () = assert (Stdlib.Sys.file_exists (path ^ "/" ^ filename)) in
  let units, data =
    let lines = Util.read_lines (path ^ filename) in
    match lines with
    | units :: data when not (List.is_empty data) -> units, data
    | _ -> failwith "Invalid data file"
  in
  let v_units, i_units = read_units units in
  let current, voltage =
    List.map data ~f:read_measurements |> Array.of_list |> Array.unzip
  in
  { current; voltage; v_units; i_units }
;;

let current_density (area : Area.t) data =
  let area_value =
    let magnitude = Prefix.value_of_prefix area.prefix |> Owl.Maths.sqr in
    magnitude *. area.value
  in
  Array.map data.current ~f:(fun i -> i /. area_value)
;;
