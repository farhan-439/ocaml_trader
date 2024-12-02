open Stock
open Rt_portfolio

val save_rt_portfolio : Rt_portfolio.rt_portfolio -> string -> unit
(** [save_rt_portfolio portfolio filename] saves the [portfolio] to the
    specified [filename] in CSV format. *)

val load_rt_portfolio : string -> Rt_portfolio.rt_portfolio option
(** [load_rt_portfolio filename] loads the portfolio from the specified
    [filename] in CSV format. Returns [Some portfolio] if the file exists, or
    [None] if the file does not exist or is malformed. *)

val serialize_rt_portfolio : Rt_portfolio.rt_portfolio -> string
(** [serialize_rt_portfolio portfolio] converts the [portfolio] into a CSV
    string format suitable for saving. *)

val deserialize_rt_portfolio : string -> Rt_portfolio.rt_portfolio ref
(** [deserialize_rt_portfolio data market] converts a CSV string back into a
    portfolio structure. *)
