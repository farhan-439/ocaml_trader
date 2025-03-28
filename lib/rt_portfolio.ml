open Api
open Lwt.Infix

type rt_portfolio = {
  balance : float;
  stocks : (string * int) list;
}

(** Abstraction Function (AF):
    - A value {balance; stocks} represents a real-time portfolio with a cash balance of [balance] and a list of stocks [stocks].
    - Each element in [stocks] is a pair (stock_name, quantity) where [stock_name] is the name of the stock and [quantity] is the number of shares owned.

    Representation Invariant (RI):
    - [balance] must be non-negative.
    - [stocks] must not contain duplicate stock names.
    - [quantity] in each (stock_name, quantity) pair must be positive.
*)

let create_rt_portfolio initial_balance =
  { balance = initial_balance; stocks = [] }

let is_stock_available stock_name =
  Api.fetch_stock_price (String.uppercase_ascii stock_name) >>= function
  | None -> Lwt.return false
  | Some api_result -> Lwt.return (Option.is_some api_result.Api.price)

let buy_stock portfolio stock_name qty =
  if qty <= 0 then Lwt.return_none
  else
    Api.fetch_stock_price stock_name >>= function
    | None -> Lwt.return_none (* Stock price not available *)
    | Some api_result -> (
        match api_result.Api.price with
        | None -> Lwt.return_none (* Price is missing in API response *)
        | Some price ->
            let total_cost = float_of_int qty *. price in
            if portfolio.balance >= total_cost then
              let updated_stocks =
                let rec update_stocks stocks =
                  match stocks with
                  | [] -> [ (stock_name, qty) ]
                  | (name, quantity) :: rest ->
                      if name = stock_name then (name, quantity + qty) :: rest
                      else (name, quantity) :: update_stocks rest
                in
                update_stocks portfolio.stocks
              in
              let updated_portfolio =
                {
                  balance = portfolio.balance -. total_cost;
                  stocks = updated_stocks;
                }
              in
              Lwt.return_some updated_portfolio
            else Lwt.return_none (* Not enough balance *))

let sell_stock portfolio stock_name qty =
  if qty <= 0 then Lwt.return_none (* Reject non-positive quantities *)
  else
    Api.fetch_stock_price stock_name >>= function
    | None -> Lwt.return_none
    | Some api_result -> (
        match api_result.Api.price with
        | None -> Lwt.return_none
        | Some price -> (
            let rec update_stocks_and_balance stocks acc_balance =
              match stocks with
              | [] -> None
              | (name, quantity) :: rest ->
                  if name = stock_name then
                    if quantity >= qty then
                      let updated_stocks =
                        if quantity = qty then rest
                        else (name, quantity - qty) :: rest
                      in
                      let total_sale = float_of_int qty *. price in
                      Some
                        {
                          balance = acc_balance +. total_sale;
                          stocks = updated_stocks;
                        }
                    else None (* Not enough shares to sell *)
                  else
                    update_stocks_and_balance rest acc_balance
                    |> Option.map (fun updated_portfolio ->
                           {
                             updated_portfolio with
                             stocks =
                               (name, quantity) :: updated_portfolio.stocks;
                           })
            in
            match
              update_stocks_and_balance portfolio.stocks portfolio.balance
            with
            | Some updated_portfolio -> Lwt.return_some updated_portfolio
            | None -> Lwt.return_none))

let rt_portfolio_summary portfolio =
  let rec stock_value_summary stocks =
    match stocks with
    | [] -> Lwt.return []
    | (name, quantity) :: rest -> (
        Api.fetch_stock_price name >>= function
        | None -> stock_value_summary rest
        | Some api_result -> (
            match api_result.Api.price with
            | None -> stock_value_summary rest
            | Some price ->
                let total_value = float_of_int quantity *. price in
                stock_value_summary rest >|= fun rest_summary ->
                (name, quantity, total_value) :: rest_summary))
  in
  stock_value_summary portfolio.stocks >|= fun stock_summary ->
  (stock_summary, portfolio.balance)

let update_rt_balance (p : rt_portfolio) (b : float) =
  { balance = b; stocks = p.stocks }

let get_balance (p : rt_portfolio) = p.balance
let get_stocks (p : rt_portfolio) = p.stocks
