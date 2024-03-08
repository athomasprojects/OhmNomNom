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
      | _ -> false)
  ;;

  let is_float = function
    | '0' .. '9' -> true
    | '.' -> true
    | '-' -> true
    | 'e' -> true
    | _ -> false
  ;;

  let float =
    take_while1 is_float
    >>| Float.of_string
    <?> "float: Parse one or more digits of floating point number"
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
