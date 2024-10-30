(* portfolio.ml *)

type portfolio = { balance : float; stocks : (string * int) list }

let create_portfolio initial_balance = 
  { balance = initial_balance; stocks = [] }

let buy_stock portfolio stock_name qty (market : Stock.t list) =
  match Stock.get_prices stock_name market with
  | [] -> None  
  | latest_price :: _ ->  
      let total_cost = float_of_int qty *. latest_price in
      if portfolio.balance >= total_cost then
        let rec update_stocks stocks =
          match stocks with
          | [] -> [(stock_name, qty)]
          | (name, quantity) :: rest ->
              if name = stock_name then
                (name, quantity + qty) :: rest
              else
                (name, quantity) :: update_stocks rest
        in
        Some { balance = portfolio.balance -. total_cost;
               stocks = update_stocks portfolio.stocks }
      else
        None

let sell_stock portfolio stock_name qty (market : Stock.t list) =
  match Stock.get_prices stock_name market with
  | [] -> None  
  | latest_price :: _ ->
      let rec find_and_update_stocks stocks =
        match stocks with
        | [] -> None
        | (name, quantity) :: rest ->
            if name = stock_name then
              if quantity >= qty then
                let updated_stocks =
                  if quantity = qty then rest
                  else (name, quantity - qty) :: rest
                in
                let total_sale = float_of_int qty *. latest_price in
                Some { balance = portfolio.balance +. total_sale;
                       stocks = updated_stocks }
              else
                None
            else
              match find_and_update_stocks rest with
              | None -> None
              | Some updated_portfolio -> Some { balance = portfolio.balance;
                                                 stocks = (name, quantity) :: updated_portfolio.stocks }
      in
      find_and_update_stocks portfolio.stocks