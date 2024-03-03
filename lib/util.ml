let is_valid x ~pred ~f ~err =
  match pred with
  | true -> Ok (f x)
  | false -> Error err
;;

let is_valid_denominator x ~f ~err =
  let open Core in
  is_valid x ~f ~pred:Float.(x > 0.) ~err:(err ^ " must be strictly positive")
;;
