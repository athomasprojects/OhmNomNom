
val create : Model.t -> [< `Semilog | `Linear] -> Matplotlib.Fig.t
val save_fig : Matplotlib.Fig.t -> Model.t -> [< `Semilog | `Linear] -> unit
