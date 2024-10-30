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
      if read_line () = "y" then (
        print_endline "Enter initial balance for the portfolio:";
        let initial_balance = float_of_string (read_line ()) in
        let portfolio = ref (create_portfolio initial_balance) in

       
        let rec portfolio_menu () =
          print_endline "\nOptions: (1) Buy stock (2) View portfolio (3) Exit";
          match read_line () with
          | "1" ->
              print_endline "Enter stock name to buy:";
              let stock_name = String.lowercase_ascii (read_line ()) in
              print_endline "Enter quantity to buy:";
              let quantity = int_of_string (read_line ()) in
              (match buy_stock !portfolio stock_name quantity new_stocks with
              | Some updated_portfolio ->
                  portfolio := updated_portfolio;
                  Printf.printf "Bought %d shares of %s\n" quantity
                    (String.capitalize_ascii stock_name)
              | None ->
                  print_endline
                    "Purchase failed. Check balance or stock availability.");
              portfolio_menu ()
          | "2" ->
              let summary, balance = portfolio_summary !portfolio new_stocks in
              Printf.printf "Current balance: %.2f\n" balance;
              List.iter
                (fun (name, qty, value) ->
                  Printf.printf "Stock: %s, Quantity: %d, Value: %.2f\n" name
                    qty value)
                summary;
              portfolio_menu ()
          | "3" -> print_endline "Exiting portfolio management."
          | _ ->
              print_endline "Invalid option. Try again.";
              portfolio_menu ()
        in
        portfolio_menu ())
      else print_endline "Portfolio creation skipped."
  | _ -> print_endline "No updates made. Goodbye!"
