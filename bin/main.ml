open Finalproject.Stock

(* Helper function to print a list of floats *)
let print_prices prices =
  List.iter (fun price -> Printf.printf "%.2f " price) prices;
  print_newline ()

let () =
  print_endline "Welcome to the Stock Query Interface!";
  print_endline
    "Please enter the filename of the stock data (e.g., data/stocks.csv):";
  let filename = read_line () in
  (* Load the stock data from the file *)
  let stock_data =
    try read_csv filename
    with _ ->
      print_endline "Error - file could not be found.";
      exit 0
  in

  (* Sample interaction: Query prices for a stock *)
  print_endline "Enter a stock name to get prices:";
  let stock_name = String.lowercase_ascii (read_line ()) in

  (try
     let prices = get_prices stock_name stock_data in
     print_endline ("Prices for " ^ String.capitalize_ascii stock_name ^ ":");
     print_prices prices
   with
  | Failure msg -> print_endline ("Error: " ^ msg)
  | Not_found -> print_endline "Stock not found.");

  (* Additional interface options *)
  print_endline "Do you want to update stock prices? (y/n)";
  match read_line () with
  | "y" | "Y" ->
      (* Apply update_prices to each element in stock_data *)
      (*This now uses a random pattern for each stock*)
      let new_stocks =
        List.map
          (let rand = Random.int 10 in
           let pattern =
             if rand < 2 then "low" else if rand < 7 then "mid" else "high"
             (*skewed slightly more towards high to offset wider low range*)
           in
           update_prices pattern)
          stock_data
      in
      (*update for all stocks*)
      List.iter
        (fun x ->
          print_string
            ((x |> to_float |> fst |> String.capitalize_ascii) ^ " Stock: ");
          (*prints the name of the stock*)
          x |> to_float |> snd |> print_prices)
        new_stocks;
      print_endline "Stock prices updated."
  | _ -> print_endline "No updates made. Goodbye!"
