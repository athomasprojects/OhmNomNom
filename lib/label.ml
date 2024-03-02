open Core

type t =
  { id : int
  ; annealed : bool
  ; pitch : int
  ; pos : int * int
  ; illum : bool
  }
[@@deriving show, eq]

let parse_label f =
  let open Parser.A in
  let is_illum = function
    | "dark" -> false
    | "light" -> true
    | _ -> assert false
  in
  let is_annealed str =
    match String.lowercase str with
    | "post" | "after" -> true
    | "pre" | "before" -> false
    | _ -> assert false
  in
  let parser =
    let* id = digit in
    let* annealed =
      umatch <* skip_string "annealing" <* underscore >>| is_annealed
    in
    let* pitch = skip (fun ch -> Char.(ch = 'p' || ch = 'P')) *> digit in
    let* pos = both (underscore *> char 'r' *> digit) (char 'c' *> digit) in
    let* illum =
      underscore *> (string "dark" <|> string "light") >>| is_illum
    in
    return { id; annealed; pitch; pos; illum }
  in
  parse_string ~consume:Prefix parser f |> Result.ok_or_failwith
;;

let string_of_label lbl =
  let string_of_illum = function
    | true -> "light"
    | false -> "dark"
  in
  let string_of_annealed = function
    | true -> "post"
    | false -> "pre"
  in
  String.concat
    ~sep:"_"
    [ Int.to_string lbl.id
    ; string_of_annealed lbl.annealed
    ; "annealing"
    ; "p" ^ Int.to_string lbl.pitch
    ; "r" ^ Int.to_string (fst lbl.pos) ^ "c" ^ Int.to_string (snd lbl.pos)
    ; string_of_illum lbl.illum
    ]
;;
