open Stock

type portfolio
(** Abstract type representing a user's portfolio, containing balance and stock
    holdings. *)

val create_portfolio : float -> portfolio
(** [create_portfolio initial_balance] initializes a portfolio with a starting
    balance and an empty stock list. *)

val buy_stock : portfolio -> string -> int -> Stock.t list -> portfolio option
(** [buy_stock portfolio stock_name qty market] allows a user to buy a specified
    quantity of a stock at the current market price. It takes the portfolio,
    stock name, quantity, and a list of available stocks in the market. Returns
    an updated portfolio or [None] if the balance is insufficient or the stock
    is not found. *)

val sell_stock : portfolio -> string -> int -> Stock.t list -> portfolio option
(** [sell_stock portfolio stock_name qty market] allows a user to sell a
    specified quantity of a stock at the current market price. It takes the
    portfolio, stock name, quantity, and a list of available stocks in the
    market. Returns an updated portfolio or [None] if the user does not have
    enough shares or the stock is not found. *)

val portfolio_summary :
  portfolio -> Stock.t list -> (string * int * float) list * float
(** [portfolio_summary portfolio market] returns a summary of the portfolio,
    including each stock with its quantity and current value based on the latest
    market prices, along with the total balance. Each stock is represented as a
    tuple of the stock name, quantity, and total value, while the balance is a
    separate float value. *)

val get_balance : portfolio -> float
(** [get_balance portfolio] returns the balance in the user's portfolio. *)

val get_stocks : portfolio -> (string * int) list
(** [get_stocks portfolio] returns the stock holdings in the user's portfolio as
    a list of (stock name, quantity). *)

val update_balance : portfolio -> float -> portfolio
(** [update_balance] artificially updates the portfolio's balance. Using this to
    fix the error while loading stocks from a portfolio that decreases balance.*)
