val create : [< `Semilog | `Linear] -> norm: bool -> Model.t -> Matplotlib.Fig.t
val create_all :  Model.t array  -> [< `Semilog | `Linear] -> norm: bool -> save: bool -> unit
val create_by_category :  [< `Row | `Col | `Pitch] -> Model.t array  -> [< `Semilog | `Linear] -> norm: bool -> save: bool -> unit
val save_fig :  [< `Semilog | `Linear] -> Model.t  -> unit
