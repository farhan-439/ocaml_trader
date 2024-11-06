open Finalproject.Stock
open Finalproject.Portfolio

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
      (*make more earnings calls here*)
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
        print_endline "Enter initial balance for the portfolio:";
        let initial_balance = float_of_string (read_line ()) in
        let portfolio = ref (create_portfolio initial_balance) in

        let rec portfolio_menu new_stocks =
          print_endline
            "\n\
             Options: (1) Buy stock (2) View portfolio (3) Exit to earnings \
             call (4) Exit program";
          match read_line () with
          | "1" ->
              print_endline "Enter stock name to buy:";
              let stock_name =
                try String.lowercase_ascii (read_line ())
                with _ ->
                  print_endline "Please try again with a string name.";
                  exit 0
              in
              print_endline "Enter quantity to buy:";
              let quantity =
                try int_of_string (read_line ())
                with _ ->
                  print_endline "Please try again with an integer quantity.";
                  exit 0
              in
              (match buy_stock !portfolio stock_name quantity new_stocks with
              | Some updated_portfolio ->
                  portfolio := updated_portfolio;
                  Printf.printf "Bought %d shares of %s\n" quantity
                    (String.capitalize_ascii stock_name)
              | None ->
                  print_endline
                    "Purchase failed. Check balance or stock availability.");
              portfolio_menu new_stocks
          | "2" ->
              let summary, balance = portfolio_summary !portfolio new_stocks in
              Printf.printf "Current balance: %.2f\n" balance;
              List.iter
                (fun (name, qty, value) ->
                  Printf.printf "Stock: %s, Quantity: %d, Value: %.2f\n" name
                    qty value)
                summary;
              let sum =
                List.fold_left
                  (fun acc (name, qty, value) -> acc +. value)
                  0. summary
                +. balance
              in
              Printf.printf "Total value (balance + stock value): %.2f" sum;
              portfolio_menu new_stocks
          | "3" -> print_endline "Simulating earnings call."
          | "4" ->
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
                 if rand < 2 then "low" else if rand < 7 then "mid" else "high"
               in

               update_prices pattern)
              stocks
          in

          List.iter
            (fun x ->
              print_string
                ((x |> to_float |> fst |> String.capitalize_ascii) ^ " Stock: ");
              x |> to_float |> snd |> print_prices)
            new_stocks;
          print_endline "Stock prices updated.";
          (*gets stock prices and updates them*)
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
        earnings_call new_stocks)
      else print_endline "Portfolio creation skipped."
  | _ -> print_endline "No updates made. Goodbye!"
