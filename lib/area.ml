open Core

type metre_t = Metre [@@deriving show, eq]

type t =
  { prefix : Prefix.t
  ; value : float
  ; quantity : metre_t
  }
[@@deriving show, eq]

let to_string t =
  Float.to_string t.value ^ " " ^ Prefix.string_of_prefix t.prefix ^ "m^2"
;;

let rec init (value, str) =
  let () = assert (Float.is_positive value) in
  let prefix = area_of_string str in
  { prefix; value; quantity = Metre }

and area_of_string s =
  let open Parser.A in
  let parser =
    let* prefix =
      take_till (fun ch ->
        match ch with
        | 'm' -> true
        | _ -> false)
      <* string "m^2"
      >>| fun t ->
      match Prefix.prefix_of_string_opt t with
      | Some p -> p
      | None -> failwith "Invalid area unit prefix."
    in
    return prefix
  in
  parse_string ~consume:Prefix parser s |> Result.ok_or_failwith
;;
