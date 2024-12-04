open Api

type rt_portfolio = {
  balance : float;
  stocks : (string * int) list;
}
(** The [rt_portfolio] type represents a user's real-time portfolio with a cash
    balance and a list of stocks held, where each stock is represented by its
    ticker and quantity. *)

val create_rt_portfolio : float -> rt_portfolio
(** [create_rt_portfolio initial_balance] initializes a rt_portfolio with a
    starting balance and an empty list of stocks. *)

val buy_stock : rt_portfolio -> string -> int -> rt_portfolio option Lwt.t
(** [buy_stock rt_portfolio stock_name qty] attempts to buy [qty] of
    [stock_name] at the current market price. Returns an updated rt_portfolio if
    the balance is sufficient, or [None] if the purchase cannot be completed due
    to insufficient funds or if the stock price cannot be retrieved. *)

val sell_stock : rt_portfolio -> string -> int -> rt_portfolio option Lwt.t
(** [sell_stock rt_portfolio stock_name qty] attempts to sell [qty] of
    [stock_name] at the current market price. Returns an updated rt_portfolio if
    the user has enough shares to sell, or [None] if the sale cannot be
    completed due to insufficient shares or if the stock price cannot be
    retrieved. *)

val rt_portfolio_summary :
  rt_portfolio -> ((string * int * float) list * float) Lwt.t
(** [rt_portfolio_summary rt_portfolio] returns a summary of the rt_portfolio,
    including each stock with its quantity and current value based on the latest
    market prices, along with the total balance. Each stock is represented as a
    tuple of the stock name, quantity, and total value, while the balance is a
    separate float value. *)

val update_rt_balance : rt_portfolio -> float -> rt_portfolio
(** [update_rt_balance] artificially updates the portfolio's balance. Using this
    to fix the error while loading stocks from a portfolio that decreases
    balance.*)

val get_balance : rt_portfolio -> float
(** [get_balance rt_portfolio] returns the balance in the user's rt_portfolio. *)

val get_stocks : rt_portfolio -> (string * int) list
(** [get_stocks rt_portfolio] returns the stock holdings in the user's
    rt_portfolio as a list of (stock name, quantity). *)
