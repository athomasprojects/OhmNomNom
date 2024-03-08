type t

val read_data : string -> string -> t
(** [read_data reads the data from the [file] located in the [path] to the directory. *)

val voltage : t -> float array
(** [voltage] returns the array of bias voltage data points in volts. *)

val current : t -> float array
(** [current] returns the array of current data points in amperes. *)

val units_of_data : [< `Current | `Voltage] -> t -> Prefix.t
(** [units_of_data qty data] returns the [data]'s unit prefix for the given [qty]. *)

val current_density : Area.t -> t -> float array
(** [current_denstity area data] normalizes the current to the device area.*)

val string_of_volts : Prefix.t -> string
(** [string_of_voltage v] returns the string representation of the voltage units. *)

val string_of_amps : Prefix.t -> string
(** [string_of_amps i] returns the string representation of the current units. *)

(** Pretty printing *)
val pp : Format.formatter -> t -> unit
val show : t -> string
val equal : t -> t -> bool
