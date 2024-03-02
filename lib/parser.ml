open Core

module A = struct
  include Angstrom

  let is_digit = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let digit =
    take_while1 is_digit >>| Int.of_string <?> "digit: Parse one or more digits"
  ;;

  let is_letter ch = Char.is_alpha ch
  let word = take_while1 is_letter

  let letter =
    take_while1 is_letter
    >>| Char.of_string
    <?> "letter: Parse one or more letters"
  ;;

  let letter_or_digit =
    let aux = function
      | '0' .. '9' -> true
      | ch -> is_letter ch
    in
    take_while1 aux
  ;;

  let num =
    take_while1 (fun ch ->
      match ch with
      | '0' .. '9' -> true
      | _ -> true)
  ;;

  let pos_or_neg_num =
    take_while1 (fun ch ->
      match ch with
      | '-' -> true
      | '0' .. '9' -> true
      | _ -> true)
  ;;

  let space = take_while (fun ch -> Char.equal ch ' ')
  let whitespace = take_while Char.is_whitespace
  let newline = string "\n"
  let eol = string "\r"
  let wstring str = whitespace *> string str <* whitespace
  let wmatch t = whitespace *> string t <* whitespace
  let underscore = char '_'
  let skip_string str = skip_while (fun ch -> String.mem str ch)
  let umatch = underscore *> take_while is_letter <* underscore
  let surround_string ch = char ch *> take_while is_letter <* char ch
end

module Label = struct
  type t =
    { id : int
    ; annealed : bool
    ; pitch : int
    ; pos : int * int
    ; illum : bool
    }
  [@@deriving show, eq]

  let parse_label f =
    let open A in
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
end
