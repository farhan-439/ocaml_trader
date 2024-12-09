open Stock
open Portfolio

val save_portfolio : Portfolio.portfolio -> Stock.t list -> string -> unit
(** [save_portfolio portfolio market filename] saves the [portfolio] and market
    data to the specified [filename] in CSV format. *)

val load_portfolio : string -> Stock.t list -> Portfolio.portfolio option
(** [load_portfolio filename market] loads the portfolio from the specified
    [filename] in CSV format. Returns [Some portfolio] if the file exists, or
    [None] if the file does not exist or is malformed. *)
