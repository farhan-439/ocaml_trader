open Stock

type portfolio
(** Abstract type representing a user's portfolio, containing balance and stock holdings. *)

val create_portfolio : float -> portfolio
(** [create_portfolio initial_balance] initializes a portfolio with a starting balance
    and an empty stock list. *)

val buy_stock : portfolio -> string -> int -> Stock.t list -> portfolio option
(** [buy_stock portfolio stock_name qty market] allows a user to buy a specified quantity 
    of a stock at the current market price. It takes the portfolio, stock name, quantity,
    and a list of available stocks in the market. Returns an updated portfolio or [None] 
    if the balance is insufficient or the stock is not found. *)

val sell_stock : portfolio -> string -> int -> Stock.t list -> portfolio option
(** [sell_stock portfolio stock_name qty market] allows a user to sell a specified quantity 
    of a stock at the current market price. It takes the portfolio, stock name, quantity,
    and a list of available stocks in the market. Returns an updated portfolio or [None] 
    if the user does not have enough shares or the stock is not found. *)

val portfolio_summary : portfolio -> (string * int * float) list * float
(** [portfolio_summary portfolio] returns a summary of the portfolio, including each stock 
    with its quantity and current value based on the latest market prices, along with the 
    total balance. *)
