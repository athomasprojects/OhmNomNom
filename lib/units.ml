type unit_type =
  [ `Voltage
  | `Current
  | `Power
  | `Unitless
  ]
[@@deriving show, eq]

type t =
  { prefix : Prefix.t
  ; unit : unit_type
  ; value : float option
  }
[@@deriving show, eq]
