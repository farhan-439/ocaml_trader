open Finalproject.Stock
open Finalproject.Save_portfolio
open Finalproject.Rt_save_portfolio
include ANSITerminal
module P = Finalproject.Portfolio
module RP = Finalproject.Rt_portfolio

let print_ascii_banner () =
  print_newline ();
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "$$$$$$\\   $$$$$$\\   $$$$$$\\  $$\\      $$\\ $$\\             \
     $$$$$$$$\\ $$$$$$$\\   $$$$$$\\  $$$$$$$\\  $$$$$$$$\\ $$$$$$$\\  \n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "$$  __$$\\ $$  __$$\\ $$  __$$\\ $$$\\    $$$ |$$ |            \\__$$  \
     __|$$  __$$\\ $$  __$$\\ $$  __$$\\ $$  _____|$$  __$$\\ \n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "$$ /  $$ |$$ /  \\__|$$ /  $$ |$$$$\\  $$$$ |$$ |               $$ |   $$ \
     |  $$ |$$ /  $$ |$$ |  $$ |$$ |      $$ |  $$ |\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "$$ |  $$ |$$ |      $$$$$$$$ |$$\\$$\\$$ $$ |$$ |               $$ |   \
     $$$$$$$  |$$$$$$$$ |$$ |  $$ |$$$$$\\    $$$$$$$  |\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "$$ |  $$ |$$ |      $$  __$$ |$$ \\$$$  $$ |$$ |               $$ |   $$  \
     __$$< $$  __$$ |$$ |  $$ |$$  __|   $$  __$$< \n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "$$ |  $$ |$$ |  $$\\ $$ |  $$ |$$ |\\$  /$$ |$$ |               $$ |   $$ \
     |  $$ |$$ |  $$ |$$ |  $$ |$$ |      $$ |  $$ |\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    " $$$$$$  |\\$$$$$$  |$$ |  $$ |$$ | \\_/ $$ |$$$$$$$$\\          $$ |   \
     $$ |  $$ |$$ |  $$ |$$$$$$$  |$$$$$$$$\\ $$ |  $$ |\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    " \\______/  \\______/ \\__|  \\__|\\__|     \\__|\\________|         \
     \\__|   \\__|  \\__|\\__|  \\__|\\_______/ \\________|\\__|  \\__|\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "                                                                                                                       \n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "                                                                                                                       \n";
  print_newline ()

let print_date_time () =
  let time = Unix.localtime (Unix.time ()) in
  let date_str =
    Printf.sprintf "📅 %02d-%02d-%04d" time.tm_mday (time.tm_mon + 1)
      (time.tm_year + 1900)
  in
  let time_str =
    Printf.sprintf "⏰ %02d:%02d:%02d" time.tm_hour time.tm_min time.tm_sec
  in
  ANSITerminal.print_string [ ANSITerminal.Bold; ANSITerminal.red ] "Date: ";
  ANSITerminal.print_string [ ANSITerminal.white ] (date_str ^ "   ");
  ANSITerminal.print_string [ ANSITerminal.Bold; ANSITerminal.green ] "Time: ";
  ANSITerminal.print_string [ ANSITerminal.white ] (time_str ^ "\n")

let print_bordered_section title =
  let border = String.make (String.length title + 4) '*' in
  ANSITerminal.print_string [ ANSITerminal.white ] (border ^ "\n");
  ANSITerminal.print_string [ ANSITerminal.white ] ("* " ^ title ^ " *\n");
  ANSITerminal.print_string [ ANSITerminal.white ] (border ^ "\n")

let print_landing_page () =
  ANSITerminal.print_string [ ANSITerminal.white ]
    "--------------------------------------------\n";
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "📊 Manage your portfolio efficiently.\n";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "💰 Buy, sell, and view stocks with ease.\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "📈 Stay up-to-date with the latest stock prices.\n";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "--------------------------------------------\n";
  ANSITerminal.print_string [ ANSITerminal.blue ] "✨ Powered by OCaml ✨\n";
  print_newline ()

let print_bordered_menu options =
  let max_length =
    List.fold_left (fun acc option -> max acc (String.length option)) 0 options
  in
  let border = String.make (max_length + 4) '-' in
  ANSITerminal.print_string [ ANSITerminal.white ] (border ^ "\n");
  List.iter
    (fun option ->
      ANSITerminal.print_string [ ANSITerminal.white ]
        ("| " ^ option
        ^ String.make (max_length - String.length option) ' '
        ^ " |\n"))
    options;
  ANSITerminal.print_string [ ANSITerminal.white ] (border ^ "\n")

let market =
  let filename = "data/stocks.csv" in
  try read_csv filename
  with _ ->
    print_endline "Error - stock data file could not be found.";
    exit 0

let print_prices prices =
  List.iter (fun price -> Printf.printf "%.2f " price) prices;
  print_newline ()

let print_help () =
  print_endline "\nAvailable Commands:";
  print_endline "1. Buy stock - Allows you to buy shares of a specific stock.";
  print_endline "2. Sell stock - Allows you to sell shares of a specific stock.";
  print_endline
    "3. View portfolio - Displays your current portfolio summary, including \
     stock holdings and balance.";
  print_endline
    "4. Exit to earnings call (simulated mode) - Updates stock prices based on \
     simulated market conditions.";
  print_endline "5. Exit program - Closes the application.";
  print_endline "6. Help - Displays this help menu.\n";
  print_endline
    "7. Save portfolio - Saves your current portfolio to a file for later use.";

  print_endline
    "Usage: Enter the number corresponding to the command you wish to execute.\n"

let print_help_rt () =
  print_endline "\nReal-Time Portfolio Commands:";
  print_endline "1. Buy stock - Buy shares of a stock in real-time.";
  print_endline "2. Sell stock - Sell shares of a stock in real-time.";
  print_endline
    "3. View portfolio - Displays your current portfolio summary, including \
     stock holdings and balance.";
  print_endline "4. Exit - Closes the real-time portfolio.";
  print_endline "5. Help - Displays this help menu.\n";
  print_endline
    "6. Save portfolio - Saves your current real-time portfolio to a file for \
     later use.";
  print_endline
    "Usage: Enter the number corresponding to the command you wish to execute.\n"

(**[balance_input_loop] takes in a mode (simulated/real-time) and prompts users
   for portfolio balance input and loops until they provide a valid input.*)
let rec balance_input_loop mode =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.cyan ]
    ("Enter initial balance for the " ^ mode ^ " portfolio:\n");
  let input = read_line () in
  try
    let balance = float_of_string input in
    balance (* Return the balance if the input is valid *)
  with _ ->
    print_endline "Invalid input - please enter a valid number.";
    balance_input_loop
      mode (* Call the function recursively if the input is invalid *)

(**[quantity_input_loop] takes in a mode (buy/sell) and prompts users to enter a
   quantity input and loops until they provide a valid input.*)
let rec quantity_input_loop mode =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.cyan ]
    ("Enter quantity to " ^ mode ^ ":\n");
  let input = read_line () in
  try
    let quantity = int_of_string input in
    quantity
  with _ ->
    print_endline "Invalid input - please enter a valid integer.";
    quantity_input_loop mode

(**[print_yes_no] prints a nicely formatted (y/n) string using ANSITerminal.*)
let print_yes_no () =
  Stdlib.print_string "(";
  ANSITerminal.print_string
    [
      ANSITerminal.green;
      ANSITerminal.on_black;
      ANSITerminal.Bold;
      ANSITerminal.Underlined;
    ]
    "y";
  Stdlib.print_string "/";
  ANSITerminal.print_string
    [
      ANSITerminal.red;
      ANSITerminal.on_black;
      ANSITerminal.Bold;
      ANSITerminal.Underlined;
    ]
    "n";
  Stdlib.print_string ") \n"

let print_num_enclosed num color =
  Stdlib.print_string "(";
  ANSITerminal.print_string
    [ color; ANSITerminal.on_black; ANSITerminal.Bold; ANSITerminal.Underlined ]
    num;
  Stdlib.print_string ")"

let print_horizontal_rule () =
  ANSITerminal.print_string [ ANSITerminal.white ] (String.make 80 '-');
  print_newline ()

(**[purchase_loop] takes in a portfolio and prompts users to enter a stock
   ticker to buy and loops until they provide a valid input.*)
let rec purchase_loop portfolio =
  print_endline "Enter stock ticker to buy: ";
  let stock_name = String.uppercase_ascii (read_line ()) in
  let quantity = quantity_input_loop "buy" in
  Printf.printf "Are you sure you want to buy %d shares of %s? " quantity
    stock_name;
  print_yes_no ();
  (*this is all setup*)
  let input = read_line () in
  if String.lowercase_ascii input = "y" then (
    try
      match Lwt_main.run (RP.buy_stock !portfolio stock_name quantity) with
      | Some updated_portfolio ->
          portfolio := updated_portfolio;
          Printf.printf "Bought %d shares of %s\n" quantity stock_name
      | None ->
          print_endline "Purchase failed. Check balance or stock availability."
    with _ ->
      print_endline ("Could not find " ^ stock_name ^ ". Please try again.");
      purchase_loop portfolio)
  else print_endline "Purchase canceled."

let () =
  ANSITerminal.erase Screen;
  ANSITerminal.set_cursor 1 1;
  print_ascii_banner ();
  print_date_time ();

  ANSITerminal.print_string
    [
      ANSITerminal.cyan;
      ANSITerminal.on_black;
      ANSITerminal.Blink;
      ANSITerminal.Inverse;
      ANSITerminal.Bold;
    ]
    "\nWelcome to the Stock Query Interface!\n\n";
  print_landing_page ();
  print_horizontal_rule ();
  Stdlib.print_string "Would you like to create a portfolio? ";
  print_yes_no ();
  if String.lowercase_ascii (read_line ()) = "y" then (
    Stdlib.print_string "Do you want to create a ";
    print_num_enclosed "1" ANSITerminal.yellow;
    Stdlib.print_string " simulated portfolio or a ";
    print_num_enclosed "2" ANSITerminal.blue;
    Stdlib.print_string " real-time portfolio? ";
    Stdlib.print_string "(";
    ANSITerminal.print_string
      [
        ANSITerminal.yellow;
        ANSITerminal.on_black;
        ANSITerminal.Bold;
        ANSITerminal.Underlined;
      ]
      "1";
    Stdlib.print_string "/";
    ANSITerminal.print_string
      [
        ANSITerminal.blue;
        ANSITerminal.on_black;
        ANSITerminal.Bold;
        ANSITerminal.Underlined;
      ]
      "2";
    Stdlib.print_string ") \n";
    match read_line () with
    | "1" ->
        (* Simulated portfolio *)
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
           print_endline
             ("Prices for " ^ String.capitalize_ascii stock_name ^ ":");
           print_prices prices
         with
        | Failure msg -> print_endline ("Error: " ^ msg)
        | Not_found -> print_endline "Stock not found.");

        let new_stocks =
          List.map
            (let rand = Random.int 10 in
             let pattern =
               if rand < 2 then "low" else if rand < 7 then "mid" else "high"
             in
             update_prices pattern)
            stock_data
        in
        print_endline "";
        List.iter
          (fun x ->
            ANSITerminal.print_string
              [
                ANSITerminal.magenta;
                ANSITerminal.on_black;
                ANSITerminal.Bold;
                ANSITerminal.Underlined;
              ]
              ((x |> to_float |> fst |> String.capitalize_ascii) ^ " Stock:");
            Stdlib.print_string " ";
            x |> to_float |> snd |> print_prices)
          new_stocks;
        print_endline "";
        print_endline "Stock prices updated.";

        let portfolio_file = "user_portfolio.json" in
        let portfolio =
          if Sys.file_exists portfolio_file then (
            Printf.printf "Existing portfolio found. Loading from %s...\n"
              portfolio_file;
            match load_portfolio portfolio_file market with
            | Some p -> ref p
            | None ->
                Printf.printf
                  "Failed to load portfolio. Starting with a new portfolio.\n";
                let initial_balance = balance_input_loop "new" in
                ref (P.create_portfolio initial_balance))
          else (
            Printf.printf
              "No saved portfolio found. Starting with a new portfolio.\n";
            let initial_balance = balance_input_loop "new" in
            ref (P.create_portfolio initial_balance))
        in

        let rec portfolio_menu new_stocks =
          print_horizontal_rule ();
          print_bordered_section "Main Menu";
          print_horizontal_rule ();
          Stdlib.print_string "\nOptions: ";
          print_num_enclosed "1" ANSITerminal.yellow;
          Stdlib.print_string " Buy stock ";
          print_num_enclosed "2" ANSITerminal.blue;
          Stdlib.print_string " Sell stock ";
          print_num_enclosed "3" ANSITerminal.magenta;
          Stdlib.print_string " View portfolio ";
          print_num_enclosed "4" ANSITerminal.cyan;
          Stdlib.print_string " Exit to earnings call ";
          print_num_enclosed "5" ANSITerminal.white;
          Stdlib.print_string " Exit program ";
          print_num_enclosed "6" ANSITerminal.green;
          Stdlib.print_string " Help ";
          print_num_enclosed "7" ANSITerminal.yellow;
          Stdlib.print_string " Save portfolio\n";

          match read_line () with
          | "1" ->
              print_endline "Enter stock name to buy:";
              let stock_name =
                try String.lowercase_ascii (read_line ())
                with _ ->
                  print_endline "Please try again with a valid stock name.";
                  exit 0
              in
              let quantity = quantity_input_loop "buy" in
              (* Confirmation step *)
              Printf.printf "Are you sure you want to buy %d shares of %s? "
                quantity
                (String.capitalize_ascii stock_name);
              print_yes_no ();
              if String.lowercase_ascii (read_line ()) = "y" then
                match P.buy_stock !portfolio stock_name quantity new_stocks with
                | Some updated_portfolio ->
                    portfolio := updated_portfolio;
                    ANSITerminal.print_string
                      [ ANSITerminal.Bold; ANSITerminal.green ]
                      (Printf.sprintf "✅ Successfully bought %d shares of %s\n"
                         quantity
                         (String.capitalize_ascii stock_name))
                | None ->
                    ANSITerminal.print_string
                      [ ANSITerminal.Bold; ANSITerminal.red ]
                      "❌ Purchase failed. Check balance or stock availability.\n"
              else
                ANSITerminal.print_string
                  [ ANSITerminal.Bold; ANSITerminal.yellow ]
                  "Purchase canceled.\n";
              print_horizontal_rule ();
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
                let quantity = quantity_input_loop "sell" in
                (* Confirmation step *)
                Printf.printf "Are you sure you want to sell %d shares of %s? "
                  quantity
                  (String.capitalize_ascii stock_name);
                print_yes_no ();
                if String.lowercase_ascii (read_line ()) = "y" then
                  match
                    P.sell_stock !portfolio stock_name quantity new_stocks
                  with
                  | Some updated_portfolio ->
                      portfolio := updated_portfolio;
                      ANSITerminal.print_string
                        [ ANSITerminal.Bold; ANSITerminal.green ]
                        (Printf.sprintf "✅ Successfully sold %d shares of %s\n"
                           quantity
                           (String.capitalize_ascii stock_name))
                  | None ->
                      ANSITerminal.print_string
                        [ ANSITerminal.Bold; ANSITerminal.red ]
                        "❌ Sale failed. Check if you have enough shares to \
                         sell or if the stock exists.\n"
                else
                  ANSITerminal.print_string
                    [ ANSITerminal.Bold; ANSITerminal.yellow ]
                    "⚠️ Sale canceled.\n";
                print_horizontal_rule ();
                portfolio_menu new_stocks
          | "3" ->
              print_horizontal_rule ();
              ANSITerminal.print_string [ ANSITerminal.blue ]
                "📊 Portfolio Summary\n";
              print_horizontal_rule ();

              let summary, balance =
                P.portfolio_summary !portfolio new_stocks
              in
              Printf.printf "Current balance: %.2f\n" balance;
              ANSITerminal.print_string [ ANSITerminal.Bold ]
                "Stock Holdings:\n";
              List.iter
                (fun (name, qty, value) ->
                  Printf.printf "Stock: %s, Quantity: %d, Value: %.2f\n" name
                    qty value)
                summary;
              let sum =
                List.fold_left
                  (fun acc (_, _, value) -> acc +. value)
                  0. summary
                +. balance
              in
              print_newline ();
              ANSITerminal.print_string [ ANSITerminal.Bold ]
                "Total Portfolio Value: ";
              ANSITerminal.print_string [ ANSITerminal.green ]
                (Printf.sprintf "%.2f\n" sum);
              print_horizontal_rule ();
              portfolio_menu new_stocks
          | "4" -> print_endline "Simulating earnings call."
          | "5" ->
              save_portfolio !portfolio market portfolio_file;
              print_endline "Portfolio saved. Exiting program. Goodbye!";
              exit 0
          | "6" ->
              print_help ();
              portfolio_menu new_stocks
          | "7" ->
              save_portfolio !portfolio market portfolio_file;
              print_endline "Portfolio saved successfully.";
              portfolio_menu new_stocks
          | _ ->
              print_endline "Invalid option. Try again.";
              portfolio_menu new_stocks
        in

        let rec earnings_call stocks =
          print_horizontal_rule ();
          ANSITerminal.print_string
            [ ANSITerminal.Bold; ANSITerminal.cyan ]
            "📈 Earnings Call Update\n";
          print_horizontal_rule ();

          let new_stocks =
            List.map
              (let rand = Random.int 10 in
               let pattern =
                 if rand < 2 then "low" else if rand < 7 then "mid" else "high"
               in
               update_prices pattern)
              stocks
          in
          print_endline "";
          List.iter
            (fun x ->
              ANSITerminal.print_string
                [
                  ANSITerminal.magenta;
                  ANSITerminal.on_black;
                  ANSITerminal.Bold;
                  ANSITerminal.Underlined;
                ]
                ((x |> to_float |> fst |> String.capitalize_ascii) ^ " Stock:");
              Stdlib.print_string " ";
              x |> to_float |> snd |> print_prices)
            new_stocks;
          print_endline "";
          print_endline "Stock prices updated.";
          (* Update portfolio or continue *)
          Stdlib.print_string
            "Would you like to update your portfolio, continue without \
             updating, or exit? ";
          Stdlib.print_string "(";
          ANSITerminal.print_string
            [
              ANSITerminal.yellow;
              ANSITerminal.on_black;
              ANSITerminal.Bold;
              ANSITerminal.Underlined;
            ]
            "1";
          Stdlib.print_string "/";
          ANSITerminal.print_string
            [
              ANSITerminal.blue;
              ANSITerminal.on_black;
              ANSITerminal.Bold;
              ANSITerminal.Underlined;
            ]
            "2";
          Stdlib.print_string "/";
          ANSITerminal.print_string
            [
              ANSITerminal.magenta;
              ANSITerminal.on_black;
              ANSITerminal.Bold;
              ANSITerminal.Underlined;
            ]
            "3";
          Stdlib.print_string ") \n";
          let input = read_line () in
          if input = "1" then (
            portfolio_menu new_stocks;
            earnings_call new_stocks)
          else if input = "2" then earnings_call new_stocks
          else exit 0
        in
        print_horizontal_rule ();
        portfolio_menu new_stocks;
        earnings_call new_stocks
    | "2" ->
        (* Real-time portfolio *)
        let portfolio_file = "user_portfolio_rt.json" in
        let portfolio =
          if Sys.file_exists portfolio_file then (
            Printf.printf "Existing portfolio found. Loading from %s...\n"
              portfolio_file;
            match load_rt_portfolio portfolio_file with
            | Some p -> ref p
            | None ->
                Printf.printf
                  "Failed to load portfolio. Starting with a new portfolio.\n";
                let initial_balance = balance_input_loop "new" in
                ref (RP.create_rt_portfolio initial_balance))
          else (
            Printf.printf
              "No saved portfolio found. Starting with a new portfolio.\n";
            let initial_balance = balance_input_loop "real-time" in
            ref (RP.create_rt_portfolio initial_balance))
        in

        let rec rt_portfolio_menu () =
          Stdlib.print_string "\nOptions: ";
          print_num_enclosed "1" ANSITerminal.yellow;
          Stdlib.print_string " Buy stock ";
          print_num_enclosed "2" ANSITerminal.blue;
          Stdlib.print_string " Sell stock ";
          print_num_enclosed "3" ANSITerminal.magenta;
          Stdlib.print_string " View portfolio ";
          print_num_enclosed "4" ANSITerminal.cyan;
          Stdlib.print_string " Exit ";
          print_num_enclosed "5" ANSITerminal.green;
          Stdlib.print_string " Help ";
          print_num_enclosed "6" ANSITerminal.yellow;
          Stdlib.print_string " Save portfolio\n";

          match read_line () with
          | "1" ->
              purchase_loop portfolio;
              rt_portfolio_menu ()
          | "2" ->
              let stocks = RP.get_stocks !portfolio in
              if stocks = [] then (
                print_endline "Error: You have no stocks to sell.";
                rt_portfolio_menu ())
              else (
                print_endline "Enter stock ticker to sell:";
                let stock_name = String.uppercase_ascii (read_line ()) in
                let quantity = quantity_input_loop "sell" in
                (* Confirmation step *)
                Printf.printf "Are you sure you want to sell %d shares of %s? "
                  quantity stock_name;
                print_yes_no ();
                if String.lowercase_ascii (read_line ()) = "y" then
                  match
                    Lwt_main.run (RP.sell_stock !portfolio stock_name quantity)
                  with
                  | Some updated_portfolio ->
                      portfolio := updated_portfolio;
                      Printf.printf "Sold %d shares of %s\n" quantity stock_name
                  | None ->
                      print_endline
                        "Sale failed. Check if you have enough shares to sell \
                         or if the stock exists."
                else print_endline "Sale canceled.";
                rt_portfolio_menu ())
          | "3" ->
              print_horizontal_rule ();
              ANSITerminal.print_string
                [ ANSITerminal.Bold; ANSITerminal.blue ]
                "📊 Real-Time Portfolio Summary\n";
              print_horizontal_rule ();
              let summary, balance =
                Lwt_main.run (RP.rt_portfolio_summary !portfolio)
              in
              Printf.printf "Current balance: %.2f\n" balance;

              ANSITerminal.print_string [ ANSITerminal.Bold ]
                "Stock Holdings:\n";
              List.iter
                (fun (name, qty, value) ->
                  Printf.printf "Stock: %s, Quantity: %d, Value: %.2f\n" name
                    qty value)
                summary;

              let sum =
                List.fold_left
                  (fun acc (_, _, value) -> acc +. value)
                  0. summary
                +. balance
              in
              print_newline ();
              ANSITerminal.print_string [ ANSITerminal.Bold ]
                "Total Portfolio Value: ";
              ANSITerminal.print_string [ ANSITerminal.green ]
                (Printf.sprintf "%.2f\n" sum);
              print_horizontal_rule ();
              rt_portfolio_menu ()
          | "4" ->
              print_endline "Exiting real-time portfolio. Goodbye!";
              exit 0
          | "5" ->
              (* Display help *)
              print_help_rt ();
              rt_portfolio_menu ()
          | "6" ->
              save_rt_portfolio !portfolio portfolio_file;
              rt_portfolio_menu ()
          (* save_portfolio !portfolio market portfolio_file; print_endline
             "Portfolio saved successfully."; *)
          | _ ->
              print_endline "Invalid option. Try again.";
              rt_portfolio_menu ()
        in

        rt_portfolio_menu ()
    | _ -> print_endline "Invalid option. Please rerun and try again.")
  else print_endline "Portfolio creation skipped. Goodbye!"
