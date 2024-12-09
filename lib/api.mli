type t = {
  ticker : string;
  price : float option;
}

val fetch_stock_price : string -> t option Lwt.t
(** [fetch_stock_price ticker] fetches the latest price for the stock with
    symbol [ticker]. Returns [Some stock] if successful, or [None] if there's an
    error. This function uses asynchronous Lwt to handle the HTTP request. *)
