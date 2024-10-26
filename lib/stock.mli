type t

val get_prices : string -> t
(**[get_prices] takes a string name of a stock and returns its price history.*)

val update_prices : string -> t -> t
(**[update_prices] takes a pattern and a stock and adds its next value using
   that pattern including some randomness. Ex: With some stock1 initialized as
   [1;2;3;4], update_prices "high" stock1 returns [1;2;3;4;7].*)

val to_float : t -> float list
(**[to_float] returns the float list representation of a certain stock.*)

val of_float : float list -> t
(**[of_float] takes some float list and returns it as a stock.*)

val read_csv : string -> t list
(**[read_csv] takes a filename with appropriate stock history and returns a list
   of the stocks represented by that csv.*)
