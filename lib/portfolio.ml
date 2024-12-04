(* portfolio.ml *)

type portfolio = {
  balance : float;
  stocks : (string * int) list;
}

let create_portfolio initial_balance =
  { balance = initial_balance; stocks = [] }

let buy_stock portfolio stock_name qty (market : Stock.t list) =
  match Stock.get_prices stock_name market with
  | [] -> None [@coverage off] (*shouldn't happen with correct implementation*)
  | prices ->
      let latest_price = List.nth prices (List.length prices - 1) in
      let total_cost = float_of_int qty *. latest_price in
      if portfolio.balance >= total_cost then
        let rec update_stocks stocks =
          match stocks with
          | [] -> [ (stock_name, qty) ]
          | (name, quantity) :: rest ->
              if name = stock_name then (name, quantity + qty) :: rest
              else (name, quantity) :: update_stocks rest
        in
        Some
          {
            balance = portfolio.balance -. total_cost;
            stocks = update_stocks portfolio.stocks;
          }
      else None

let sell_stock portfolio stock_name qty (market : Stock.t list) =
  match Stock.get_prices stock_name market with
  | [] -> None [@coverage off] (*shouldn't happen with correct implementation*)
  | prices ->
      let latest_price = List.nth prices (List.length prices - 1) in
      let rec update_stocks_and_balance stocks acc_balance =
        match stocks with
        | [] -> None
        | (name, quantity) :: rest -> (
            if name = stock_name then
              if quantity >= qty then
                let updated_stocks =
                  if quantity = qty then rest
                  else (name, quantity - qty) :: rest
                in
                let total_sale = float_of_int qty *. latest_price in
                Some
                  {
                    balance = acc_balance +. total_sale;
                    stocks = updated_stocks;
                  }
              else None
            else
              match update_stocks_and_balance rest acc_balance with
              | None -> None
              | Some updated_portfolio ->
                  Some
                    {
                      balance = updated_portfolio.balance;
                      stocks = (name, quantity) :: updated_portfolio.stocks;
                    })
      in
      update_stocks_and_balance portfolio.stocks portfolio.balance

let portfolio_summary portfolio (market : Stock.t list) =
  let rec stock_value_summary stocks =
    match stocks with
    | [] -> []
    | (name, quantity) :: rest -> (
        match Stock.get_prices name market with
        | [] ->
            stock_value_summary rest
            [@coverage off]
            (*shouldn't happen with correct implementation*)
            (* Stock not found in market, skip *)
        | prices ->
            let latest_price = List.nth prices (List.length prices - 1) in
            let total_value = float_of_int quantity *. latest_price in
            (name, quantity, total_value) :: stock_value_summary rest)
  in
  (stock_value_summary portfolio.stocks, portfolio.balance)

let update_balance (p : portfolio) (b : float) =
  { balance = b; stocks = p.stocks }

let get_balance (p : portfolio) = p.balance
let get_stocks (p : portfolio) = p.stocks
