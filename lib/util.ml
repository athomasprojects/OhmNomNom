open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let text = In_channel.input_all channel in
    String.split_lines text)
;;

let range_seq start stop =
  let next i = if i > stop then None else Some (i, i + 1) in
  Seq.unfold next start
;;

let range_seq_offset start stop ~offset =
  let next i = if i > stop then None else Some (i, i + offset) in
  Seq.unfold next start
;;

let is_valid x ~pred ~f ~err =
  match pred with
  | true -> Ok (f x)
  | false -> Error err
;;

let is_valid_denominator x ~f ~err =
  let open Core in
  is_valid x ~f ~pred:Float.(x > 0.) ~err:(err ^ " must be strictly positive")
;;
