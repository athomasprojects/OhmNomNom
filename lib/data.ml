open Core

type t =
  { current : float array
  ; voltage : float array
  ; v_units : Prefix.t
  ; i_units : Prefix.t
  }
[@@deriving show, eq]

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
;;

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
;;

let read_units str =
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

let read_data path file =
  let lines = Parser.read_lines (path ^ file) in
  let units, data =
    match lines with
    | units :: data -> units, data
    | _ -> failwith "Invalid data file"
  in
  let v_units, i_units = read_units units in
  let current, voltage =
    Array.of_list data |> Array.map ~f:read_measurements |> Array.unzip
  in
  { current; voltage; v_units; i_units }
;;

let current_density (area : Area.t) data =
  Array.map data.current ~f:(fun i -> i /. area.value)
;;
