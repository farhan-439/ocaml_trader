open Finalproject.Stock

module P = Finalproject.Portfolio
module RP = Finalproject.Rt_portfolio

let print_prices prices =
  List.iter (fun price -> Printf.printf "%.2f " price) prices;
  print_newline ()

let () =
  print_endline "Welcome to the Stock Query Interface!";
  print_endline
    "Please enter the filename of the stock data (e.g., data/stocks.csv):";
  let filename = read_line () in
  let stock_data =
    try read_csv filename
    with _ ->
      print_endline "Error - file could not be found.";
      exit 0
  in

  print_endline "Enter a stock name to get prices:";
  let stock_name = String.lowercase_ascii (read_line ()) in

  (try
     let prices = get_prices stock_name stock_data in
     print_endline ("Prices for " ^ String.capitalize_ascii stock_name ^ ":");
     print_prices prices
   with
  | Failure msg -> print_endline ("Error: " ^ msg)
  | Not_found -> print_endline "Stock not found.");

  print_endline
    "Do you want to update stock prices after the earnings call and make a \
     portfolio? (y/n)";

  match read_line () with
  | "y" | "Y" ->
      let new_stocks =
        List.map
          (let rand = Random.int 10 in
           let pattern =
             if rand < 2 then "low" else if rand < 7 then "mid" else "high"
           in
           update_prices pattern)
          stock_data
      in

      List.iter
        (fun x ->
          print_string
            ((x |> to_float |> fst |> String.capitalize_ascii) ^ " Stock: ");
          x |> to_float |> snd |> print_prices)
        new_stocks;
      print_endline "Stock prices updated.";

      (* Portfolio functionality starts here *)
      print_endline "Would you like to create a portfolio? (y/n)";
      if String.lowercase_ascii (read_line ()) = "y" then (
        print_endline
          "Do you want to create a (1) simulated portfolio or a (2) real-time \
           portfolio? (1/2)";
        match read_line () with
        | "1" ->
            (* Simulated portfolio *)
            print_endline "Enter initial balance for the portfolio:";
            let initial_balance = float_of_string (read_line ()) in
            let portfolio = ref (P.create_portfolio initial_balance) in

            let rec portfolio_menu new_stocks =
              print_endline
                "\n\
                 Options: (1) Buy stock (2) Sell stock (3) View portfolio (4) \
                 Exit to earnings call (5) Exit program";
              match read_line () with
              | "1" ->
                  print_endline "Enter stock name to buy:";
                  let stock_name =
                    try String.lowercase_ascii (read_line ())
                    with _ ->
                      print_endline "Please try again with a valid stock name.";
                      exit 0
                  in
                  print_endline "Enter quantity to buy:";
                  let quantity =
                    try int_of_string (read_line ())
                    with _ ->
                      print_endline
                        "Please try again with a valid integer quantity.";
                      exit 0
                  in
                  (* Confirmation step *)
                  Printf.printf
                    "Are you sure you want to buy %d shares of %s? (y/n)\n"
                    quantity
                    (String.capitalize_ascii stock_name);
                  if String.lowercase_ascii (read_line ()) = "y" then
                    match
                      P.buy_stock !portfolio stock_name quantity new_stocks
                    with
                    | Some updated_portfolio ->
                        portfolio := updated_portfolio;
                        Printf.printf "Bought %d shares of %s\n" quantity
                          (String.capitalize_ascii stock_name)
                    | None ->
                        print_endline
                          "Purchase failed. Check balance or stock availability."
                  else print_endline "Purchase canceled.";
                  portfolio_menu new_stocks
              | "2" ->
                  let stocks = P.get_stocks !portfolio in
                  if stocks = [] then (
                    print_endline "Error: You have no stocks to sell.";
                    portfolio_menu new_stocks)
                  else
                    let stock_name =
                      match stocks with
                      | [ (name, _) ] ->
                          print_endline
                            ("Assuming stock to sell is: "
                            ^ String.capitalize_ascii name);
                          name
                      | _ ->
                          print_endline "Enter stock name to sell:";
                          String.lowercase_ascii (read_line ())
                    in
                    print_endline "Enter quantity to sell:";
                    let quantity = int_of_string (read_line ()) in
                    (* Confirmation step *)
                    Printf.printf
                      "Are you sure you want to sell %d shares of %s? (y/n)\n"
                      quantity
                      (String.capitalize_ascii stock_name);
                    if String.lowercase_ascii (read_line ()) = "y" then
                      match
                        P.sell_stock !portfolio stock_name quantity new_stocks
                      with
                      | Some updated_portfolio ->
                          portfolio := updated_portfolio;
                          Printf.printf "Sold %d shares of %s\n" quantity
                            (String.capitalize_ascii stock_name)
                      | None ->
                          print_endline
                            "Sale failed. Check if you have enough shares to \
                             sell or if the stock exists."
                    else print_endline "Sale canceled.";
                    portfolio_menu new_stocks
              | "3" ->
                  let summary, balance =
                    P.portfolio_summary !portfolio new_stocks
                  in
                  Printf.printf "Current balance: %.2f\n" balance;
                  List.iter
                    (fun (name, qty, value) ->
                      Printf.printf "Stock: %s, Quantity: %d, Value: %.2f\n"
                        name qty value)
                    summary;
                  let sum =
                    List.fold_left
                      (fun acc (_, _, value) -> acc +. value)
                      0. summary
                    +. balance
                  in
                  Printf.printf "Total value (balance + stock value): %.2f\n" sum;
                  portfolio_menu new_stocks
              | "4" -> print_endline "Simulating earnings call."
              | "5" ->
                  print_endline "Exiting program. Goodbye!";
                  exit 0
              | _ ->
                  print_endline "Invalid option. Try again.";
                  portfolio_menu new_stocks
            in

            let rec earnings_call stocks =
              let new_stocks =
                List.map
                  (let rand = Random.int 10 in
                   let pattern =
                     if rand < 2 then "low"
                     else if rand < 7 then "mid"
                     else "high"
                   in
                   update_prices pattern)
                  stocks
              in

              List.iter
                (fun x ->
                  print_string
                    ((x |> to_float |> fst |> String.capitalize_ascii)
                    ^ " Stock: ");
                  x |> to_float |> snd |> print_prices)
                new_stocks;
              print_endline "Stock prices updated.";
              (* Update portfolio or continue *)
              print_endline
                "Would you like to update your portfolio, continue without \
                 updating, or exit? (1/2/3)";
              let input = read_line () in
              if input = "1" then (
                portfolio_menu new_stocks;
                earnings_call new_stocks)
              else if input = "2" then earnings_call new_stocks
              else exit 0
            in

            portfolio_menu new_stocks;
            earnings_call new_stocks
        | "2" ->
            (* Real-time portfolio *)
            print_endline "Enter initial balance for the real-time portfolio:";
            let initial_balance = float_of_string (read_line ()) in
            let portfolio = ref (RP.create_rt_portfolio initial_balance) in

            let rec rt_portfolio_menu () =
              print_endline
                "\nOptions: (1) Buy stock (2) Sell stock (3) View portfolio (4) \
                 Exit";
              match read_line () with
              | "1" ->
                  print_endline "Enter stock ticker to buy:";
                  let stock_name = String.uppercase_ascii (read_line ()) in
                  print_endline "Enter quantity to buy:";
                  let quantity = int_of_string (read_line ()) in
                  (* Confirmation step *)
                  Printf.printf
                    "Are you sure you want to buy %d shares of %s? (y/n)\n"
                    quantity stock_name;
                  if String.lowercase_ascii (read_line ()) = "y" then
                    match Lwt_main.run (RP.buy_stock !portfolio stock_name quantity) with
                    | Some updated_portfolio ->
                        portfolio := updated_portfolio;
                        Printf.printf "Bought %d shares of %s\n" quantity
                          stock_name
                    | None ->
                        print_endline
                          "Purchase failed. Check balance or stock availability."
                  else print_endline "Purchase canceled.";
                  rt_portfolio_menu ()
              | "2" ->
                  let stocks = RP.get_stocks !portfolio in
                  if stocks = [] then (
                    print_endline "Error: You have no stocks to sell.";
                    rt_portfolio_menu ())
                  else (
                    print_endline "Enter stock ticker to sell:";
                    let stock_name = String.uppercase_ascii (read_line ()) in
                    print_endline "Enter quantity to sell:";
                    let quantity = int_of_string (read_line ()) in
                    (* Confirmation step *)
                    Printf.printf
                      "Are you sure you want to sell %d shares of %s? (y/n)\n"
                      quantity stock_name;
                    if String.lowercase_ascii (read_line ()) = "y" then
                      match
                        Lwt_main.run (RP.sell_stock !portfolio stock_name quantity)
                      with
                      | Some updated_portfolio ->
                          portfolio := updated_portfolio;
                          Printf.printf "Sold %d shares of %s\n" quantity
                            stock_name
                      | None ->
                          print_endline
                            "Sale failed. Check if you have enough shares to \
                             sell or if the stock exists."
                    else print_endline "Sale canceled.";
                    rt_portfolio_menu ())
              | "3" ->
                  let summary, balance = Lwt_main.run (RP.rt_portfolio_summary !portfolio) in
                  Printf.printf "Current balance: %.2f\n" balance;
                  List.iter
                    (fun (name, qty, value) ->
                      Printf.printf "Stock: %s, Quantity: %d, Value: %.2f\n"
                        name qty value)
                    summary;
                  let sum =
                    List.fold_left
                      (fun acc (_, _, value) -> acc +. value)
                      0. summary
                    +. balance
                  in
                  Printf.printf "Total value (balance + stock value): %.2f\n"
                    sum;
                  rt_portfolio_menu ()
              | "4" ->
                  print_endline "Exiting real-time portfolio. Goodbye!";
                  exit 0
              | _ ->
                  print_endline "Invalid option. Try again.";
                  rt_portfolio_menu ()
            in

            rt_portfolio_menu ()
        | _ ->
            print_endline "Invalid option. Skipping portfolio creation.")
      else print_endline "Portfolio creation skipped."
  | _ -> print_endline "No updates made. Goodbye!"
