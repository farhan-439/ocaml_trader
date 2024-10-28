open Finalproject.Stock

(* Helper function to print a list of floats *)
let print_prices prices =
  List.iter (fun price -> Printf.printf "%.2f " price) prices;
  print_newline ()

let () =
  print_endline "Welcome to the Stock Query Interface!";
  print_endline "Please enter the filename of the stock data (e.g., stock.csv):";
  let filename = read_line () in

  (* Load the stock data from the file *)
  let stock_data = read_csv filename in
  print_endline "Stock data loaded.";

  (* Sample interaction: Query prices for a stock *)
  print_endline "Enter a stock name to get prices:";
  let stock_name = read_line () in

  (try
     let prices = get_prices stock_name in
     let float_prices = to_float prices in
     (* Convert prices to float list *)
     print_endline ("Prices for " ^ stock_name ^ ":");
     print_prices float_prices
   with
  | Failure msg -> print_endline ("Error: " ^ msg)
  | Not_found -> print_endline "Stock not found.");

  (* Additional interface options *)
  print_endline "Do you want to update stock prices? (y/n)";
  match read_line () with
  | "y" | "Y" ->
      print_endline "Enter the pattern for stocks to update:";
      let pattern = read_line () in
      (* Apply update_prices to each element in stock_data *)
      let _ = List.map (update_prices pattern) stock_data in
      print_endline "Stock prices updated."
  | _ -> print_endline "No updates made. Goodbye!"
