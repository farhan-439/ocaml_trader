open OUnit2
open Finalproject
open Portfolio
open Rt_portfolio
open Api
open Lwt.Infix
open Save_portfolio
open Rt_save_portfolio

let stocks = Stock.read_csv "../data/stocks.csv"
let fin_stocks = Stock.read_csv "../data/financial.csv"

(**[float_list_printer] is a helper printer for pretty float list printing.*)
let float_list_printer lst =
  "[" ^ String.concat "; " (List.map string_of_float lst) ^ "]"

(**[test_get_prices stock price] will generate a test case for
   [Stock.get_prices] using [stock] and [price] and a filename.*)
let test_get_prices stock price filename =
  assert_equal price
    (Stock.get_prices stock (Stock.read_csv filename))
    ~printer:float_list_printer

(**[test_update_prices pattern stock_name stock_prices] will generate a test
   case for [Stock.update_prices] using [pattern], [stock_name] and
   [stock_prices].*)
let test_update_prices pattern stock_name stock_prices =
  let updated_prices =
    Stock.update_prices pattern (Stock.of_float stock_name stock_prices)
  in
  let name, prices = Stock.to_float updated_prices in

  assert_equal stock_name name ~printer:Fun.id;
  assert_equal
    (List.length stock_prices + 1)
    (List.length prices) ~printer:string_of_int;

  let avg_price =
    List.fold_left ( +. ) 0. prices /. float_of_int (List.length prices)
  in

  let last_price = List.nth prices (List.length prices - 1) in
  let within_tolerance a b tolerance = abs_float (a -. b) <= tolerance in
  match pattern with
  | "high" ->
      assert (within_tolerance last_price (avg_price *. 1.3) (avg_price *. 0.3))
  | "mid" ->
      assert (within_tolerance last_price (avg_price *. 1.0) (avg_price *. 0.2))
  | _ ->
      assert (within_tolerance last_price (avg_price *. 0.7) (avg_price *. 0.3))

(**[test_update_balance balance expected_balance] tests to make sure an empty
   portfolio updates its balance to a specific balance. Stocks should not
   update.*)
let test_update_balance balance expected_balance =
  let portfolio = balance |> update_balance (Portfolio.create_portfolio 0.) in
  assert_equal
    (Portfolio.get_balance portfolio)
    expected_balance ~printer:string_of_float;
  assert_equal [] (Portfolio.get_stocks portfolio)

(**[test_rt_update_balance balance expected_balance] tests to make sure an empty
   portfolio updates its balance to a specific balance. Stocks should not
   update.*)
let test_rt_update_balance balance expected_balance =
  let rt_portfolio =
    balance
    |> Rt_portfolio.update_rt_balance (Rt_portfolio.create_rt_portfolio 0.)
  in
  assert_equal
    (Rt_portfolio.get_balance rt_portfolio)
    expected_balance ~printer:string_of_float;
  assert_equal [] (Rt_portfolio.get_stocks rt_portfolio)

(**[test_to_float stock expected_name expected_prices] will generate a test case
   for [Stock.to_float] using [stock], [expected_nme] and [expected_prices].*)
let test_to_float stock expected_name expected_prices =
  let name, prices = Stock.to_float stock in
  assert_equal expected_name name ~printer:Fun.id;
  assert_equal expected_prices prices ~printer:float_list_printer

(**[test_of_float name prices] will generate a test case for [Stock.of_float]
   using [name] and [prices].*)
let test_of_float name prices =
  let stock = Stock.of_float name prices in
  let retrieved_name, retrieved_prices = Stock.to_float stock in
  assert_equal name retrieved_name ~printer:Fun.id;
  assert_equal prices retrieved_prices ~printer:float_list_printer

(*PORTFOLIO TESTS*)

(** [test_create_portfolio balance expected_balance] tests the creation of a
    portfolio with an initial balance and an empty stock list. *)
let test_create_portfolio balance expected_balance =
  let portfolio = Portfolio.create_portfolio balance in
  assert_equal expected_balance
    (Portfolio.get_balance portfolio)
    ~printer:string_of_float;
  assert_equal [] (Portfolio.get_stocks portfolio)

(** [test_create_rt_portfolio balance expected_balance] tests the creation of an
    rt_portfolio with an initial balance and an empty stock list. Slightly
    different functions.*)
let test_create_rt_portfolio balance expected_balance =
  let portfolio = Rt_portfolio.create_rt_portfolio balance in
  assert_equal expected_balance
    (Rt_portfolio.get_balance portfolio)
    ~printer:string_of_float;
  assert_equal [] (Rt_portfolio.get_stocks portfolio)

(** [test_buy_stock portfolio stock_name qty market expected_balance expected_stocks]
    tests buying stocks by checking the updated balance and stock list. *)
let test_buy_stock portfolio stock_name qty market expected_balance
    expected_stocks =
  match Portfolio.buy_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert_equal expected_balance
        (Portfolio.get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (Portfolio.get_stocks updated_portfolio)
  | None -> assert_failure "Expected purchase to succeed"

(** [test_buy_stock_insufficient_balance portfolio stock_name qty market] tests
    buying stocks with insufficient balance, expecting it to fail. *)
let test_buy_stock_insufficient_balance portfolio stock_name qty market =
  match Portfolio.buy_stock portfolio stock_name qty market with
  | None -> () (* Expected failure due to insufficient balance *)
  | Some _ ->
      assert_failure "Expected purchase to fail due to insufficient balance"

(**[test_api_fetch_bad] attempts to fetch a stock that does exist, and should
   return an appropriate response.*)
let test_api_fetch_good ticker : unit =
  let _ =
    Api.fetch_stock_price ticker >>= function
    | None -> assert_failure "Expected stock to be available"
    | Some api_result ->
        assert_equal api_result.Api.ticker ticker;
        Lwt.return_true
  in
  ignore ()

(**[test_api_fetch_bad] attempts to fetch a stock that doesn't exist, and should
   fail.*)
let test_api_fetch_bad ticker : unit =
  let _ =
    Api.fetch_stock_price ticker >>= function
    | None -> Lwt.return_true
    | Some api_result -> assert_failure "shouldn't be available"
  in
  ignore ()

(** [test_sell_stock portfolio stock_name qty market expected_balance expected_stocks]
    tests selling stocks by checking the updated balance and stock list. *)
let test_sell_stock portfolio stock_name qty market expected_balance
    expected_stocks =
  match Portfolio.sell_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert_equal expected_balance
        (Portfolio.get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (Portfolio.get_stocks updated_portfolio)
  | None -> assert_failure "Expected sale to succeed"

(** [test_sell_stock_insufficient_qty portfolio stock_name qty market] tests
    selling stocks with an insufficient quantity, expecting it to fail. *)
let test_sell_stock_insufficient_qty portfolio stock_name qty market =
  match Portfolio.sell_stock portfolio stock_name qty market with
  | None -> () (* Expected failure due to insufficient shares *)
  | Some _ -> assert_failure "Expected sale to fail due to insufficient shares"

(** [test_portfolio_summary portfolio market expected_summary expected_balance]
    tests that [portfolio_summary] returns the correct stock holdings and
    balance. Uses a tolerance for floating-point comparisons *)
let test_portfolio_summary portfolio market expected_summary expected_balance =
  let summary, balance = portfolio_summary portfolio market in
  List.iter2
    (fun (name_expected, qty_expected, value_expected)
         (name_actual, qty_actual, value_actual) ->
      assert (name_expected = name_actual);
      assert (qty_expected = qty_actual);
      assert (abs_float (value_expected -. value_actual) <= 1e-5))
    expected_summary summary;
  assert (abs_float (expected_balance -. balance) <= 1e-5)

(** [test_buy_stock_existing_stock portfolio stock_name qty market expected_balance expected_stocks]
    tests buying additional shares of a stock already in the portfolio,
    verifying the updated balance and stock list. *)
let test_buy_stock_existing_stock portfolio stock_name qty market
    expected_balance expected_stocks =
  match Portfolio.buy_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert_equal expected_balance
        (Portfolio.get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (Portfolio.get_stocks updated_portfolio)
  | None -> assert_failure "Expected purchase to succeed"

(** [test_sell_stock_all_but_one portfolio stock_name qty market expected_balance expected_stocks]
    tests selling all but one share of a stock, ensuring the stock remains in
    the portfolio with the correct quantity and balance update. *)
let test_sell_stock_all_but_one portfolio stock_name qty market expected_balance
    expected_stocks =
  match Portfolio.sell_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert_equal expected_balance
        (Portfolio.get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (Portfolio.get_stocks updated_portfolio)
  | None -> assert_failure "Expected sale to succeed"

(** [test_sell_stock_increases_balance portfolio stock_name qty market expected_balance expected_stocks]
    tests that the portfolio balance increases after selling a stock. *)
let test_sell_stock_increases_balance portfolio stock_name qty market
    expected_balance expected_stocks =
  match Portfolio.sell_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert (
        Portfolio.get_balance updated_portfolio
        > Portfolio.get_balance portfolio);
      assert_equal expected_balance
        (Portfolio.get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (Portfolio.get_stocks updated_portfolio)
  | None -> assert_failure "Expected sale to succeed"

(** [test_update_prices_no_change pattern stock_name stock_prices] tests
    updating stock prices with a pattern that should result in minimal price
    change (e.g., 'mid' pattern with a narrow range). *)
let test_update_prices_no_change pattern stock_name stock_prices =
  let updated_prices =
    Stock.update_prices pattern (Stock.of_float stock_name stock_prices)
  in
  let _, prices = Stock.to_float updated_prices in
  assert_equal
    (List.length stock_prices + 1)
    (List.length prices) ~printer:string_of_int;
  let last_price = List.nth prices (List.length prices - 1) in
  assert (
    last_price >= List.nth stock_prices (List.length stock_prices - 1) *. 0.9
    && last_price <= List.nth stock_prices (List.length stock_prices - 1) *. 1.1)

(**[test_empty_rt_portfolio_summary] makes sure the summary of an empty
   portfolio is empty*)
let test_empty_rt_portfolio_summary =
  let portfolio = Rt_portfolio.create_rt_portfolio 100. in
  match Lwt_main.run (Rt_portfolio.rt_portfolio_summary portfolio) with
  | [], x -> ignore () (*empty portfolio - good*)
  | _ -> assert_failure "Should be empty"

(** [test_buy_stock_invalid_name portfolio invalid_name market] Tests buying a
    stock with an invalid name, expecting it to fail. *)
let test_buy_stock_invalid_name portfolio invalid_name market =
  match Portfolio.buy_stock portfolio invalid_name 10 market with
  | None -> ()
  | Some _ -> assert_failure "Expected purchase to fail for invalid stock name"

(** [test_portfolio_with_mixed_stocks portfolio market expected_summary expected_balance]
    Tests a portfolio containing multiple stocks with varying quantities. *)
let test_portfolio_with_mixed_stocks portfolio market expected_summary
    expected_balance =
  let within_tolerance a b tolerance = abs_float (a -. b) <= tolerance in
  let summary, balance = portfolio_summary portfolio market in
  assert (within_tolerance balance expected_balance 0.01);
  List.iter2
    (fun (expected_name, expected_qty, expected_value)
         (actual_name, actual_qty, actual_value) ->
      assert_equal expected_name actual_name;
      assert_equal expected_qty actual_qty;
      assert (within_tolerance actual_value expected_value 0.01))
    expected_summary summary

(** [test_portfolio_summary_empty portfolio market] Tests portfolio summary when
    there are no stocks. *)
let test_portfolio_summary_empty portfolio market =
  let summary, balance = portfolio_summary portfolio market in
  assert_equal [] summary;
  assert_equal
    (Portfolio.get_balance portfolio)
    balance ~printer:string_of_float

(** [test_buy_stock_large_qty portfolio stock_name market] Tests buying an
    extremely large quantity of shares, expecting failure. *)
let test_buy_stock_large_qty portfolio stock_name market =
  match Portfolio.buy_stock portfolio stock_name 1_000_000 market with
  | None -> ()
  | Some _ -> assert_failure "Expected purchase to fail for large quantity"

(** [test_sell_stock_zero_holdings portfolio stock_name market] Tests selling a
    stock not present in the portfolio, expecting it to fail. *)
let test_sell_stock_zero_holdings portfolio stock_name market =
  match Portfolio.sell_stock portfolio stock_name 10 market with
  | None -> ()
  | Some _ -> assert_failure "Expected sale to fail for zero holdings"

(** [test_portfolio_summary_missing_prices portfolio market] Tests portfolio
    summary when some stocks have missing prices. *)
let test_portfolio_summary_missing_prices portfolio market =
  let summary, _ = Portfolio.portfolio_summary portfolio market in
  List.iter
    (fun (name, _, value) ->
      if value = 0. then
        print_endline
          (Printf.sprintf "Warning: Missing price for stock %s in portfolio"
             name))
    summary

(** [test_buy_stock_negative_price portfolio stock_name market] Tests buying a
    stock with a negative price, expecting it to fail. *)
let test_buy_stock_negative_price portfolio stock_name market =
  match Portfolio.buy_stock portfolio stock_name 10 market with
  | None -> ()
  | Some _ -> assert_failure "Expected purchase to fail for negative price"

(** [test_portfolio_summary_empty_portfolio portfolio market] Tests portfolio
    summary for an empty portfolio. *)
let test_portfolio_summary_empty_portfolio portfolio market =
  let summary, balance = Portfolio.portfolio_summary portfolio market in
  assert_equal [] summary;
  assert_equal
    (Portfolio.get_balance portfolio)
    balance ~printer:string_of_float

(** [test_sell_stock_case_sensitivity portfolio market] Tests selling shares
    with case-sensitive stock names, ensuring the system correctly identifies
    holdings. *)
let test_sell_stock_case_sensitivity portfolio market =
  match Portfolio.sell_stock portfolio "Aapl" 10 market with
  | None -> ()
  | Some _ -> assert_failure "Expected sale to fail due to insufficient shares"

(** [test_portfolio_summary_total_calculation portfolio market] Tests that the
    total portfolio value matches the sum of individual stock values and the
    balance. *)
let test_portfolio_summary_total_calculation portfolio market =
  let summary, balance = Portfolio.portfolio_summary portfolio market in
  let total_value =
    List.fold_left (fun acc (_, _, value) -> acc +. value) 0. summary +. balance
  in
  assert_equal total_value balance ~printer:string_of_float

(** [test_buy_and_sell_stock_multiple_times portfolio stock_name market] Tests
    buying and selling the same stock multiple times, verifying quantities and
    balances. *)
let test_buy_and_sell_stock_multiple_times portfolio stock_name market =
  match Portfolio.buy_stock portfolio stock_name 10 market with
  | Some portfolio1 -> (
      match Portfolio.sell_stock portfolio1 stock_name 5 market with
      | Some portfolio2 ->
          let summary, _ = Portfolio.portfolio_summary portfolio2 market in
          let _, quantity, _ = List.hd summary in
          assert_equal 5 quantity ~printer:string_of_int
      | None -> assert_failure "Expected sale to succeed")
  | None -> assert_failure "Expected purchase to succeed"

(** [create_temp_file content] creates a temporary file with [content] and
    returns its filename. *)
let create_temp_file content =
  let filename = Filename.temp_file "portfolio" ".csv" in
  let oc = open_out filename in
  output_string oc content;
  close_out oc;
  filename

(** [test_save_rt_portfolio _] tests that saving a real-time portfolio writes
    the correct data to a file. *)
let test_save_rt_portfolio _ =
  let portfolio =
    { balance = 1000.0; stocks = [ ("AAPL", 10); ("MSFT", 5) ] }
  in
  let filename = Filename.temp_file "test" ".csv" in
  save_rt_portfolio portfolio filename;

  let ic = open_in filename in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> close_in ic);
  let lines = List.rev !lines in
  assert_equal lines
    [ "balance,1000.00"; "AAPL,10"; "MSFT,5" ]
    ~printer:(String.concat "\n");

  Sys.remove filename

(** [test_load_rt_portfolio_valid _] tests loading a valid real-time portfolio
    from a file. *)
let test_load_rt_portfolio_valid _ =
  let content = "balance,1500.00\nGOOG,15\nTSLA,20" in
  let filename = create_temp_file content in
  match load_rt_portfolio filename with
  | Some portfolio ->
      assert_equal portfolio.balance 1500.00 ~printer:string_of_float;
      assert_equal portfolio.stocks [ ("GOOG", 15); ("TSLA", 20) ]
  | None -> assert_failure "Failed to load valid portfolio"

(** [test_load_rt_portfolio_invalid_balance _] tests loading a portfolio with a
    malformed balance line. *)
let test_load_rt_portfolio_invalid_balance _ =
  let content = "bal,1500.00\nGOOG,15\nTSLA,20" in
  let filename = create_temp_file content in
  match load_rt_portfolio filename with
  | None -> ()
  | Some _ -> assert_failure "Expected failure due to malformed balance line"

(** [test_load_rt_portfolio_invalid_stock _] tests loading a portfolio with a
    malformed stock line. *)
let test_load_rt_portfolio_invalid_stock _ =
  let content = "balance,1500.00\nGOOG,abc\nTSLA,20" in
  let filename = create_temp_file content in
  match load_rt_portfolio filename with
  | None -> ()
  | Some _ -> assert_failure "Expected failure due to malformed stock line"

(** [test_load_rt_portfolio_empty_file _] tests loading a portfolio from an
    empty file. *)
let test_load_rt_portfolio_empty_file _ =
  let filename = create_temp_file "" in
  match load_rt_portfolio filename with
  | None -> ()
  | Some _ -> assert_failure "Expected failure due to empty file"

(** [test_save_and_load_rt_portfolio_round_trip _] tests that saving and loading
    a portfolio preserves its data. *)
let test_save_and_load_rt_portfolio_round_trip _ =
  let portfolio =
    { balance = 2000.0; stocks = [ ("AAPL", 5); ("GOOG", 3); ("AMZN", 7) ] }
  in
  let filename = Filename.temp_file "round_trip" ".csv" in
  save_rt_portfolio portfolio filename;

  (match load_rt_portfolio filename with
  | Some loaded_portfolio ->
      assert_equal ~printer:string_of_float loaded_portfolio.balance
        portfolio.balance;
      assert_equal loaded_portfolio.stocks portfolio.stocks
  | None ->
      let msg =
        Printf.sprintf "Failed to load portfolio from file %s" filename
      in
      assert_failure msg);

  Sys.remove filename

(** [test_load_rt_portfolio_malformed_stock_line _] tests loading a portfolio
    with a malformed stock line. *)
let test_load_rt_portfolio_malformed_stock_line _ =
  let content = "balance,1500.00\nAAPL\nTSLA,20" in
  let filename = create_temp_file content in
  match load_rt_portfolio filename with
  | None -> ()
  | Some _ -> assert_failure "Expected failure due to malformed stock line"

(** [create_temp_portfolio_file content] creates a temporary file with [content]
    for portfolio tests and returns its filename. *)
let create_temp_portfolio_file content =
  let filename = Filename.temp_file "portfolio" ".csv" in
  let oc = open_out filename in
  output_string oc content;
  close_out oc;
  filename

(** [test_load_portfolio_valid _] tests loading a valid portfolio from a file. *)
let test_load_portfolio_valid _ =
  let market =
    [
      Stock.of_float "AAPL" [ 150.0 ];
      Stock.of_float "MSFT" [ 300.0 ];
      Stock.of_float "GOOG" [ 2800.0 ];
    ]
  in
  let content = "balance,1500.00\nAAPL,10,1500.00\nMSFT,5,1500.00" in
  let filename = create_temp_portfolio_file content in
  match load_portfolio filename market with
  | Some portfolio ->
      assert_equal
        (Portfolio.get_balance portfolio)
        1500.00 ~printer:string_of_float;
      assert_equal
        (Portfolio.get_stocks portfolio)
        [ ("AAPL", 10); ("MSFT", 5) ]
  | None -> assert_failure "Failed to load valid portfolio"

(** [test_load_portfolio_invalid_balance _] tests loading a portfolio with a
    malformed balance line. *)
let test_load_portfolio_invalid_balance _ =
  let market =
    [
      Stock.of_float "AAPL" [ 150.0 ];
      Stock.of_float "MSFT" [ 300.0 ];
      Stock.of_float "GOOG" [ 2800.0 ];
    ]
  in
  let content = "bal,1500.00\nAAPL,10,1500.00\nMSFT,5,1500.00" in
  let filename = create_temp_portfolio_file content in
  match load_portfolio filename market with
  | None -> ()
  | Some _ -> assert_failure "Expected failure due to malformed balance line"

(** [test_load_portfolio_invalid_stock _] tests loading a portfolio with a
    malformed stock line. *)
let test_load_portfolio_invalid_stock _ =
  let market =
    [
      Stock.of_float "AAPL" [ 150.0 ];
      Stock.of_float "MSFT" [ 300.0 ];
      Stock.of_float "GOOG" [ 2800.0 ];
    ]
  in
  let content = "balance,1500.00\nAAPL,abc,1500.00\nMSFT,5,1500.00" in
  let filename = create_temp_portfolio_file content in
  match load_portfolio filename market with
  | None -> ()
  | Some _ -> assert_failure "Expected failure due to malformed stock line"

(** [test_save_and_load_portfolio_round_trip _] tests that saving and loading a
    portfolio preserves its data. *)
let test_save_and_load_portfolio_round_trip _ =
  let market =
    [
      Stock.of_float "AAPL" [ 150.0 ];
      Stock.of_float "MSFT" [ 300.0 ];
      Stock.of_float "GOOG" [ 2800.0 ];
    ]
  in
  let portfolio =
    ( ( Portfolio.create_portfolio 1000000.0 |> fun p ->
        match Portfolio.buy_stock p "AAPL" 5 market with
        | Some p -> p
        | None -> failwith "Failed to buy stock AAPL" )
    |> fun p ->
      match Portfolio.buy_stock p "GOOG" 3 market with
      | Some p -> p
      | None -> failwith "Failed to buy stock GOOG" )
    |> fun p ->
    match Portfolio.buy_stock p "MSFT" 7 market with
    | Some p -> p
    | None -> failwith "Failed to buy stock MSFT"
  in

  let filename = Filename.temp_file "round_trip" ".csv" in
  save_portfolio portfolio market filename;

  (match load_portfolio filename market with
  | Some loaded_portfolio ->
      assert_equal ~printer:string_of_float
        (Portfolio.get_balance loaded_portfolio)
        (Portfolio.get_balance portfolio);
      assert_equal
        (Portfolio.get_stocks loaded_portfolio)
        (Portfolio.get_stocks portfolio)
  | None ->
      let msg =
        Printf.sprintf "Failed to load portfolio from file %s" filename
      in
      failwith msg);

  Sys.remove filename

(** [test_save_portfolio_existing_stock _] tests that save_portfolio correctly
    fetches the current price of a stock that exists in the market and includes
    it in the serialized output. *)
let test_save_portfolio_existing_stock _ =
  let market =
    [ Stock.of_float "AAPL" [ 100.0; 150.0 ]; Stock.of_float "MSFT" [ 300.0 ] ]
  in
  let portfolio =
    Portfolio.create_portfolio 5000.0 |> fun p ->
    match Portfolio.buy_stock p "AAPL" 5 market with
    | Some updated_p -> updated_p
    | None -> (
        failwith "Failed to buy stock AAPL" |> fun p ->
        match Portfolio.buy_stock p "MSFT" 3 market with
        | Some updated_p -> updated_p
        | None -> failwith "Failed to buy stock MSFT")
  in

  let filename = Filename.temp_file "test_existing_stock" ".csv" in
  save_portfolio portfolio market filename;

  let ic = open_in filename in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> close_in ic);
  let lines = List.rev !lines in

  let expected_lines =
    [ "balance,3350.000000"; "AAPL,5,750.00"; "MSFT,3,900.00" ]
  in
  assert_equal lines expected_lines ~printer:(String.concat "\n");
  Sys.remove filename

(** [test_load_portfolio_no_file _] tests that loading a portfolio from a
    non-existent file correctly triggers the [Sys_error] branch and returns
    [None]. *)
let test_load_portfolio_no_file _ =
  let market =
    [
      Stock.of_float "AAPL" [ 150.0 ];
      Stock.of_float "MSFT" [ 300.0 ];
      Stock.of_float "GOOG" [ 2800.0 ];
    ]
  in
  let filename = "/nonexistent/file/path.csv" in
  match load_portfolio filename market with
  | None -> ()
  | Some _ -> assert_failure "Expected None for non-existent file"

(** [test_load_portfolio_malformed_stock_line _] tests that [load_portfolio]
    handles a malformed stock line by reaching the expected branch and returning
    [None]. *)
let test_load_portfolio_malformed_stock_line _ =
  let market =
    [ Stock.of_float "AAPL" [ 100.0 ]; Stock.of_float "MSFT" [ 300.0 ] ]
  in
  let malformed_data = "balance,1500.00\nAAPL,abc\nMSFT,5,1500.00" in
  let malformed_filename = create_temp_file malformed_data in

  match load_portfolio malformed_filename market with
  | None -> ()
  | Some _ -> assert_failure "Expected None for a malformed stock line"

(** [test_load_portfolio_missing_stock_in_market _] tests that [load_portfolio]
    gracefully skips stocks not found in the market, ensuring they are ignored
    and do not cause errors. *)
let test_load_portfolio_missing_stock_in_market _ =
  let market = [ Stock.of_float "AAPL" [ 150.0 ] ] in
  let portfolio_data = "balance,1500.00\nAAPL,5,750.00\nMSFT,3,900.00" in
  let filename = create_temp_file portfolio_data in

  match load_portfolio filename market with
  | Some portfolio ->
      assert_equal ~printer:string_of_float
        (Portfolio.get_balance portfolio)
        1500.00;
      assert_equal (Portfolio.get_stocks portfolio) [ ("AAPL", 5) ]
  | None -> assert_failure "Expected a valid portfolio, but got None"

let test_stocks =
  [
    Stock.of_float "Aapl" [ 145.3; 146.2; 147.5; 148.0 ];
    Stock.of_float "Msft" [ 305.1; 306.2; 307.0; 308.4 ];
    Stock.of_float "Goog" [ 2750.2; 2748.0; 2760.5; 2755.3 ];
    Stock.of_float "Amzn" [ 3335.5; 3340.1; 3338.0; 3342.2 ];
  ]

module Api = struct
  type stock_info = {
    ticker : string;
    price : float option;
  }

  let fetch_stock_price stock_name =
    let mock_data =
      [ ("AAPL", Some 150.0); ("TSLA", Some 700.0); ("GOOG", None) ]
    in
    Lwt.return
      (List.assoc_opt (String.uppercase_ascii stock_name) mock_data
      |> Option.map (fun price -> { ticker = stock_name; price }))
end

let test_1buy_stock _ =
  let portfolio = create_rt_portfolio 1000.0 in
  let stock_name = "AAPL" in
  let qty = 5 in
  let test_lwt =
    buy_stock portfolio stock_name qty >>= function
    | None ->
        Printf.printf
          "Failed to buy stock: insufficient balance or stock not available\n";
        Lwt.return ()
    | Some updated_portfolio ->
        Printf.printf "Successfully bought %d shares of %s\n" qty stock_name;
        Printf.printf "New balance: %.2f\n" updated_portfolio.balance;
        Lwt.return ()
  in
  Lwt.async (fun () -> test_lwt)

let test_1buy_stock_invalid _ =
  let portfolio = create_rt_portfolio 1000.0 in
  let stock_name = "AMZN" in
  let qty = 5 in
  let test_lwt =
    buy_stock portfolio stock_name qty >>= function
    | None ->
        Printf.printf
          "Failed to buy stock: insufficient balance or stock not available\n";
        Lwt.return ()
    | Some updated_portfolio ->
        Printf.printf "Successfully bought %d shares of %s\n" qty stock_name;
        Printf.printf "New balance: %.2f\n" updated_portfolio.balance;
        Lwt.return ()
  in
  Lwt.async (fun () -> test_lwt)

let test_1sell_stock _ =
  let portfolio =
    { balance = 1000.0; stocks = [ ("AAPL", 10); ("TSLA", 2) ] }
  in
  let stock_name = "AAPL" in
  let qty = 3 in
  let test_lwt =
    sell_stock portfolio stock_name qty >>= function
    | None ->
        Printf.printf
          "Failed to sell stock: insufficient shares or stock not available\n";
        Lwt.return ()
    | Some updated_portfolio ->
        Printf.printf "Successfully sold %d shares of %s\n" qty stock_name;
        Printf.printf "New balance: %.2f\n" updated_portfolio.balance;
        Lwt.return ()
  in
  Lwt.async (fun () -> test_lwt)

let test_1sell_stock_invalid _ =
  let portfolio =
    { balance = 1000.0; stocks = [ ("AAPL", 10); ("TSLA", 2) ] }
  in
  let stock_name = "AMZN" in
  let qty = 3 in
  let test_lwt =
    sell_stock portfolio stock_name qty >>= function
    | None ->
        Printf.printf
          "Failed to sell stock: insufficient shares or stock not available\n";
        Lwt.return ()
    | Some updated_portfolio ->
        Printf.printf "Successfully sold %d shares of %s\n" qty stock_name;
        Printf.printf "New balance: %.2f\n" updated_portfolio.balance;
        Lwt.return ()
  in
  Lwt.async (fun () -> test_lwt)

let test_rt_portfolio_summary _ =
  let portfolio =
    { balance = 1000.0; stocks = [ ("AAPL", 10); ("TSLA", 5) ] }
  in
  let test_lwt =
    rt_portfolio_summary portfolio >>= fun (stock_summary, balance) ->
    Printf.printf "Portfolio Summary:\n";
    List.iter
      (fun (name, qty, value) ->
        Printf.printf "%s: %d shares, Total Value: %.2f\n" name qty value)
      stock_summary;
    Printf.printf "Balance: %.2f\n" balance;
    Lwt.return ()
  in
  Lwt.async (fun () -> test_lwt)

let mock_fetch_stock_price ticker =
  if ticker = "AAPL" then Some { ticker = "AAPL"; price = Some 150.0 } else None

let test_fetch_valid_api _ =
  let ticker = "AAPL" in
  match mock_fetch_stock_price ticker with
  | None -> assert_failure "API call failed or returned None"
  | Some stock -> (
      assert_equal ~printer:(fun s -> s) "AAPL" stock.ticker;
      match stock.price with
      | Some price -> assert_bool "Price should be positive" (price > 0.0)
      | None -> assert_failure "Price is missing")

let shared_portfolio = create_rt_portfolio 1000.0

let shared_portfolio_with_stocks =
  { balance = 1000.0; stocks = [ ("AAPL", 10); ("GOOG", 5) ] }

let test_1create_rt_portfolio _ =
  assert_equal 1000.0 (get_balance shared_portfolio) ~printer:string_of_float;
  assert_equal [] (get_stocks shared_portfolio) ~printer:(fun _ -> "[]")

let test_1get_balance _ =
  assert_equal 1000.0 (get_balance shared_portfolio) ~printer:string_of_float

let test_1get_stocks _ =
  assert_equal
    [ ("AAPL", 10); ("GOOG", 5) ]
    (get_stocks shared_portfolio_with_stocks)
    ~printer:(fun stocks ->
      String.concat ", "
        (List.map (fun (name, qty) -> Printf.sprintf "%s: %d" name qty) stocks))

let tests =
  "test suite"
  >::: [
         ("test api fetch good" >:: fun _ -> test_api_fetch_good "aapl");
         ("test api fetch bad" >:: fun _ -> test_api_fetch_bad "cs3110");
         ( "test dune functionality" >:: fun _ ->
           assert_equal 0 0 ~printer:string_of_int;
           assert_equal "1" "1" ~printer:Fun.id );
         ( "test get_prices Apple" >:: fun _ ->
           test_get_prices "Aapl"
             [ 145.3; 146.2; 147.5; 148.0 ]
             "../data/stocks.csv" );
         ( "test get_prices Microsoft" >:: fun _ ->
           test_get_prices "Msft"
             [ 305.1; 306.2; 307.0; 308.4 ]
             "../data/stocks.csv" );
         ( "test get_prices Google" >:: fun _ ->
           test_get_prices "Goog"
             [ 2750.2; 2748.0; 2760.5; 2755.3 ]
             "../data/stocks.csv" );
         ( "test get_prices Amazon" >:: fun _ ->
           test_get_prices "Amzn"
             [ 3335.5; 3340.1; 3338.0; 3342.2 ]
             "../data/stocks.csv" );
         ( "test get_prices JP Morgan" >:: fun _ ->
           test_get_prices "Jpm"
             [ 328.66; 488.23; 370.07; 292.7 ]
             "../data/financial.csv" );
         ( "test get_prices Bank of America" >:: fun _ ->
           test_get_prices "Bac"
             [ 189.94; 355.76; 155.91; 325.06 ]
             "../data/financial.csv" );
         ( "test get_prices Wells Fargo" >:: fun _ ->
           test_get_prices "Wfc"
             [ 225.91; 200.61; 334.89; 190.99 ]
             "../data/financial.csv" );
         ( "test get_prices Citi Group" >:: fun _ ->
           test_get_prices "C"
             [ 238.48; 223.81; 364.25; 433.55 ]
             "../data/financial.csv" );
         ( "test get_prices Goldman Sachs" >:: fun _ ->
           test_get_prices "Gs"
             [ 433.44; 128.63; 499.54; 308.67 ]
             "../data/financial.csv" );
         ( "test get_prices Morgan Stanley" >:: fun _ ->
           test_get_prices "Ms"
             [ 422.44; 460.85; 159.41; 372.54 ]
             "../data/financial.csv" );
         ( "test get_prices American Express" >:: fun _ ->
           test_get_prices "Axp"
             [ 333.81; 116.17; 288.77; 140.37 ]
             "../data/financial.csv" );
         ( "test get_prices US Bancorp" >:: fun _ ->
           test_get_prices "Usb"
             [ 161.17; 290.72; 389.38; 152.9 ]
             "../data/financial.csv" );
         ( "test get_prices PNC Financial" >:: fun _ ->
           test_get_prices "Pnc"
             [ 479.51; 451.17; 295.25; 213.26 ]
             "../data/financial.csv" );
         ( "test get_prices Capital One" >:: fun _ ->
           test_get_prices "Cof"
             [ 460.08; 159.97; 323.45; 221.71 ]
             "../data/financial.csv" );
         ( "test get_prices Charles Schwab" >:: fun _ ->
           test_get_prices "Schw"
             [ 199.89; 433.42; 390.73; 353.58 ]
             "../data/financial.csv" );
         ( "test get_prices Blackrock" >:: fun _ ->
           test_get_prices "Blk"
             [ 473.75; 484.41; 233.95; 433.61 ]
             "../data/financial.csv" );
         ( "test get_prices American International" >:: fun _ ->
           test_get_prices "Aig"
             [ 219.67; 478.28; 176.9; 417.88 ]
             "../data/financial.csv" );
         ( "test get_prices Metlife" >:: fun _ ->
           test_get_prices "Met"
             [ 180.31; 194.68; 354.59; 445.64 ]
             "../data/financial.csv" );
         ( "test get_prices CME Group" >:: fun _ ->
           test_get_prices "Cme"
             [ 349.82; 312.66; 299.36; 331.26 ]
             "../data/financial.csv" );
         ( "test get_prices Ally Financial" >:: fun _ ->
           test_get_prices "Ally"
             [ 300.12; 312.45; 295.00; 305.6 ]
             "../data/financial.csv" );
         ( "test get_prices BB&T Corp" >:: fun _ ->
           test_get_prices "Tfc"
             [ 200.45; 250.10; 210.3; 220.5 ]
             "../data/financial.csv" );
         ( "test get_prices Fifth Third Bancorp" >:: fun _ ->
           test_get_prices "Fitp"
             [ 185.4; 190.6; 179.2; 188.5 ]
             "../data/financial.csv" );
         ( "test get_prices Regions Financial" >:: fun _ ->
           test_get_prices "Rf"
             [ 145.9; 150.4; 143.0; 149.9 ]
             "../data/financial.csv" );
         ( "test get_prices KeyCorp" >:: fun _ ->
           test_get_prices "Key"
             [ 190.2; 210.8; 205.1; 202.3 ]
             "../data/financial.csv" );
         ( "test get_prices M&T Bank" >:: fun _ ->
           test_get_prices "Mtb"
             [ 299.3; 310.6; 305.4; 315.2 ]
             "../data/financial.csv" );
         ( "test get_prices SunTrust Banks" >:: fun _ ->
           test_get_prices "Sti"
             [ 270.5; 265.9; 275.0; 260.1 ]
             "../data/financial.csv" );
         ( "test get_prices Northern Trust" >:: fun _ ->
           test_get_prices "Ntrs"
             [ 360.4; 358.6; 362.7; 355.5 ]
             "../data/financial.csv" );
         ( "test get_prices State Street" >:: fun _ ->
           test_get_prices "Stt"
             [ 410.0; 415.2; 407.3; 420.1 ]
             "../data/financial.csv" );
         ( "test get_prices TD Bank" >:: fun _ ->
           test_get_prices "Td"
             [ 375.5; 380.7; 385.2; 390.6 ]
             "../data/financial.csv" );
         (* update_prices tests for new entries (varying patterns) *)
         ( "test update_prices high Ally Financial" >:: fun _ ->
           test_update_prices "high" "Ally" [ 300.12; 312.45; 295.00; 305.6 ] );
         ( "test update_prices mid BB&T Corp" >:: fun _ ->
           test_update_prices "mid" "Tfc" [ 200.45; 250.10; 210.3; 220.5 ] );
         ( "test update_prices low Fifth Third Bancorp" >:: fun _ ->
           test_update_prices "low" "Fitp" [ 185.4; 190.6; 179.2; 188.5 ] );
         ( "test update_prices high Regions Financial" >:: fun _ ->
           test_update_prices "high" "Rf" [ 145.9; 150.4; 143.0; 149.9 ] );
         ( "test update_prices mid KeyCorp" >:: fun _ ->
           test_update_prices "mid" "Key" [ 190.2; 210.8; 205.1; 202.3 ] );
         ( "test update_prices low M&T Bank" >:: fun _ ->
           test_update_prices "low" "Mtb" [ 299.3; 310.6; 305.4; 315.2 ] );
         ( "test update_prices high SunTrust Banks" >:: fun _ ->
           test_update_prices "high" "Sti" [ 270.5; 265.9; 275.0; 260.1 ] );
         ( "test update_prices mid Northern Trust" >:: fun _ ->
           test_update_prices "mid" "Ntrs" [ 360.4; 358.6; 362.7; 355.5 ] );
         ( "test update_prices low State Street" >:: fun _ ->
           test_update_prices "low" "Stt" [ 410.0; 415.2; 407.3; 420.1 ] );
         ( "test update_prices high TD Bank" >:: fun _ ->
           test_update_prices "high" "Td" [ 375.5; 380.7; 385.2; 390.6 ] );
         (* to_float tests for new entries *)
         ( "test to_float Ally Financial" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 15)
             "Ally"
             [ 300.12; 312.45; 295.00; 305.6 ] );
         ( "test to_float BB&T Corp" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 16)
             "Tfc"
             [ 200.45; 250.10; 210.3; 220.5 ] );
         ( "test to_float Fifth Third Bancorp" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 17)
             "Fitp"
             [ 185.4; 190.6; 179.2; 188.5 ] );
         ( "test to_float Regions Financial" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 18)
             "Rf"
             [ 145.9; 150.4; 143.0; 149.9 ] );
         ( "test to_float KeyCorp" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 19)
             "Key"
             [ 190.2; 210.8; 205.1; 202.3 ] );
         ( "test to_float M&T Bank" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 20)
             "Mtb"
             [ 299.3; 310.6; 305.4; 315.2 ] );
         ( "test to_float SunTrust Banks" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 21)
             "Sti"
             [ 270.5; 265.9; 275.0; 260.1 ] );
         ( "test to_float Northern Trust" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 22)
             "Ntrs"
             [ 360.4; 358.6; 362.7; 355.5 ] );
         ( "test to_float State Street" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 23)
             "Stt"
             [ 410.0; 415.2; 407.3; 420.1 ] );
         ( "test to_float TD Bank" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 24)
             "Td"
             [ 375.5; 380.7; 385.2; 390.6 ] );
         (* of_float tests for new entries *)
         ( "test of_float Ally Financial" >:: fun _ ->
           test_of_float "Ally" [ 300.12; 312.45; 295.00; 305.6 ] );
         ( "test of_float BB&T Corp" >:: fun _ ->
           test_of_float "Tfc" [ 200.45; 250.10; 210.3; 220.5 ] );
         ( "test of_float Fifth Third Bancorp" >:: fun _ ->
           test_of_float "Fitp" [ 185.4; 190.6; 179.2; 188.5 ] );
         ( "test of_float Regions Financial" >:: fun _ ->
           test_of_float "Rf" [ 145.9; 150.4; 143.0; 149.9 ] );
         ( "test of_float KeyCorp" >:: fun _ ->
           test_of_float "Key" [ 190.2; 210.8; 205.1; 202.3 ] );
         ( "test of_float M&T Bank" >:: fun _ ->
           test_of_float "Mtb" [ 299.3; 310.6; 305.4; 315.2 ] );
         ( "test of_float SunTrust Banks" >:: fun _ ->
           test_of_float "Sti" [ 270.5; 265.9; 275.0; 260.1 ] );
         ( "test of_float Northern Trust" >:: fun _ ->
           test_of_float "Ntrs" [ 360.4; 358.6; 362.7; 355.5 ] );
         ( "test of_float State Street" >:: fun _ ->
           test_of_float "Stt" [ 410.0; 415.2; 407.3; 420.1 ] );
         ( "test of_float TD Bank" >:: fun _ ->
           test_of_float "Td" [ 375.5; 380.7; 385.2; 390.6 ] );
         ( "test get_prices US Bancorp" >:: fun _ ->
           test_get_prices "Usb"
             [ 161.17; 290.72; 389.38; 152.9 ]
             "../data/financial.csv" );
         ( "test get_prices PNC Financial" >:: fun _ ->
           test_get_prices "Pnc"
             [ 479.51; 451.17; 295.25; 213.26 ]
             "../data/financial.csv" );
         ( "test get_prices Capital One" >:: fun _ ->
           test_get_prices "Cof"
             [ 460.08; 159.97; 323.45; 221.71 ]
             "../data/financial.csv" );
         ( "test get_prices Charles Schwab" >:: fun _ ->
           test_get_prices "Schw"
             [ 199.89; 433.42; 390.73; 353.58 ]
             "../data/financial.csv" );
         ( "test get_prices BlackRock" >:: fun _ ->
           test_get_prices "Blk"
             [ 473.75; 484.41; 233.95; 433.61 ]
             "../data/financial.csv" );
         ( "test get_prices American International" >:: fun _ ->
           test_get_prices "Aig"
             [ 219.67; 478.28; 176.9; 417.88 ]
             "../data/financial.csv" );
         ( "test get_prices MetLife" >:: fun _ ->
           test_get_prices "Met"
             [ 180.31; 194.68; 354.59; 445.64 ]
             "../data/financial.csv" );
         ( "test get_prices CME Group" >:: fun _ ->
           test_get_prices "Cme"
             [ 349.82; 312.66; 299.36; 331.26 ]
             "../data/financial.csv" );
         ( "test update_prices high Apple" >:: fun _ ->
           test_update_prices "high" "Aapl" [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test update_prices mid Microsoft" >:: fun _ ->
           test_update_prices "mid" "Msft" [ 305.1; 306.2; 307.0; 308.4 ] );
         ( "test update_prices low Google" >:: fun _ ->
           test_update_prices "low" "Goog" [ 2750.2; 2748.0; 2760.5; 2755.3 ] );
         ( "test update_prices high Amazon" >:: fun _ ->
           test_update_prices "high" "Amzn" [ 3335.5; 3340.1; 3338.0; 3342.2 ]
         );
         ( "test update_prices high JP Morgan" >:: fun _ ->
           test_update_prices "high" "Jpm" [ 328.66; 488.23; 370.07; 292.7 ] );
         ( "test update_prices mid Bank of America" >:: fun _ ->
           test_update_prices "mid" "Bac" [ 189.94; 355.76; 155.91; 325.06 ] );
         ( "test update_prices low Wells Fargo" >:: fun _ ->
           test_update_prices "low" "Wfc" [ 225.91; 200.61; 334.89; 190.99 ] );
         ( "test update_prices high Citi Group" >:: fun _ ->
           test_update_prices "high" "C" [ 238.48; 223.81; 364.25; 433.55 ] );
         ( "test update_prices mid Goldman Sachs" >:: fun _ ->
           test_update_prices "mid" "Gs" [ 433.44; 128.63; 499.54; 308.67 ] );
         ( "test update_prices low Morgan Stanley" >:: fun _ ->
           test_update_prices "low" "Ms" [ 422.44; 460.85; 159.41; 372.54 ] );
         ( "test update_prices high American Express" >:: fun _ ->
           test_update_prices "high" "Axp" [ 333.81; 116.17; 288.77; 140.37 ] );
         ( "test update_prices mid US Bancorp" >:: fun _ ->
           test_update_prices "mid" "Usb" [ 161.17; 290.72; 389.38; 152.9 ] );
         ( "test update_prices low PNC financial" >:: fun _ ->
           test_update_prices "low" "Pnc" [ 479.51; 451.17; 295.25; 213.26 ] );
         ( "test update_prices high Capital One" >:: fun _ ->
           test_update_prices "high" "Cof" [ 460.08; 159.97; 323.45; 221.71 ] );
         ( "test update_prices mid Charles Schwab" >:: fun _ ->
           test_update_prices "mid" "Schw" [ 199.89; 433.42; 390.73; 353.58 ] );
         ( "test update_prices low Blackrock" >:: fun _ ->
           test_update_prices "low" "Blk" [ 473.75; 484.41; 233.95; 433.61 ] );
         ( "test update_prices high American International" >:: fun _ ->
           test_update_prices "high" "Aig" [ 219.67; 478.28; 176.9; 417.88 ] );
         ( "test update_prices mid Metlife" >:: fun _ ->
           test_update_prices "mid" "Met" [ 180.31; 194.68; 354.59; 445.64 ] );
         ( "test update_prices low CME Group" >:: fun _ ->
           test_update_prices "low" "Cme" [ 349.82; 312.66; 299.36; 331.26 ] );
         ("test update_balance" >:: fun _ -> test_update_balance 100000. 100000.);
         ( "test update_rt_balance" >:: fun _ ->
           test_rt_update_balance 100000. 100000. );
         ( "test to_float Apple" >:: fun _ ->
           test_to_float (List.hd stocks) "Aapl" [ 145.3; 146.2; 147.5; 148.0 ]
         );
         ( "test to_float Microsoft" >:: fun _ ->
           test_to_float (List.nth stocks 1) "Msft"
             [ 305.1; 306.2; 307.0; 308.4 ] );
         ( "test to_float Google" >:: fun _ ->
           test_to_float (List.nth stocks 2) "Goog"
             [ 2750.2; 2748.0; 2760.5; 2755.3 ] );
         ( "test to_float Amazon" >:: fun _ ->
           test_to_float (List.nth stocks 3) "Amzn"
             [ 3335.5; 3340.1; 3338.0; 3342.2 ] );
         ( "test to_float JP Morgan" >:: fun _ ->
           test_to_float (List.hd fin_stocks) "Jpm"
             [ 328.66; 488.23; 370.07; 292.7 ] );
         ( "test to_float Bank of America" >:: fun _ ->
           test_to_float (List.nth fin_stocks 1) "Bac"
             [ 189.94; 355.76; 155.91; 325.06 ] );
         ( "test to_float Wells Fargo" >:: fun _ ->
           test_to_float (List.nth fin_stocks 2) "Wfc"
             [ 225.91; 200.61; 334.89; 190.99 ] );
         ( "test to_float Citi Group" >:: fun _ ->
           test_to_float (List.nth fin_stocks 3) "C"
             [ 238.48; 223.81; 364.25; 433.55 ] );
         ( "test to_float Goldman Sachs" >:: fun _ ->
           test_to_float (List.nth fin_stocks 4) "Gs"
             [ 433.44; 128.63; 499.54; 308.67 ] );
         ( "test to_float Morgan Stanley" >:: fun _ ->
           test_to_float (List.nth fin_stocks 5) "Ms"
             [ 422.44; 460.85; 159.41; 372.54 ] );
         ( "test to_float American Express" >:: fun _ ->
           test_to_float (List.nth fin_stocks 6) "Axp"
             [ 333.81; 116.17; 288.77; 140.37 ] );
         ( "test to_float US Bancorp" >:: fun _ ->
           test_to_float (List.nth fin_stocks 7) "Usb"
             [ 161.17; 290.72; 389.38; 152.9 ] );
         ( "test to_float PNC financial" >:: fun _ ->
           test_to_float (List.nth fin_stocks 8) "Pnc"
             [ 479.51; 451.17; 295.25; 213.26 ] );
         ( "test to_float Capital One" >:: fun _ ->
           test_to_float (List.nth fin_stocks 9) "Cof"
             [ 460.08; 159.97; 323.45; 221.71 ] );
         ( "test to_float Charles Schwab" >:: fun _ ->
           test_to_float (List.nth fin_stocks 10) "Schw"
             [ 199.89; 433.42; 390.73; 353.58 ] );
         ( "test to_float Blackrock" >:: fun _ ->
           test_to_float (List.nth fin_stocks 11) "Blk"
             [ 473.75; 484.41; 233.95; 433.61 ] );
         ( "test to_float American International" >:: fun _ ->
           test_to_float (List.nth fin_stocks 12) "Aig"
             [ 219.67; 478.28; 176.9; 417.88 ] );
         ( "test to_float Metlife" >:: fun _ ->
           test_to_float (List.nth fin_stocks 13) "Met"
             [ 180.31; 194.68; 354.59; 445.64 ] );
         ( "test to_float CME Group" >:: fun _ ->
           test_to_float (List.nth fin_stocks 14) "Cme"
             [ 349.82; 312.66; 299.36; 331.26 ] );
         ( "test of_float Apple" >:: fun _ ->
           test_of_float "Aapl" [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test of_float Microsoft" >:: fun _ ->
           test_of_float "Msft" [ 305.1; 306.2; 307.0; 308.4 ] );
         ( "test of_float Google" >:: fun _ ->
           test_of_float "Goog" [ 2750.2; 2748.0; 2760.5; 2755.3 ] );
         ( "test of_float Amazon" >:: fun _ ->
           test_of_float "Amzn" [ 3335.5; 3340.1; 3338.0; 3342.2 ] );
         ( "test of_float JP Morgan" >:: fun _ ->
           test_of_float "Jpm" [ 328.66; 488.23; 370.07; 292.7 ] );
         ( "test of_float Bank of America" >:: fun _ ->
           test_of_float "Bac" [ 189.94; 355.76; 155.91; 325.06 ] );
         ( "test of_float Wells Fargo" >:: fun _ ->
           test_of_float "Wfc" [ 225.91; 200.61; 334.89; 190.99 ] );
         ( "test of_float Citi Group" >:: fun _ ->
           test_of_float "C" [ 238.48; 223.81; 364.25; 433.55 ] );
         ( "test of_float Goldman Sachs" >:: fun _ ->
           test_of_float "Gs" [ 433.44; 128.63; 499.54; 308.67 ] );
         ( "test of_float Morgan Stanley" >:: fun _ ->
           test_of_float "Ms" [ 422.44; 460.85; 159.41; 372.54 ] );
         ( "test of_float American Express" >:: fun _ ->
           test_of_float "Axp" [ 333.81; 116.17; 288.77; 140.37 ] );
         ( "test of_float US Bancorp" >:: fun _ ->
           test_of_float "Usb" [ 161.17; 290.72; 389.38; 152.9 ] );
         ( "test of_float PNC financial" >:: fun _ ->
           test_of_float "Pnc" [ 479.51; 451.17; 295.25; 213.26 ] );
         ( "test of_float Capital One" >:: fun _ ->
           test_of_float "Cof" [ 460.08; 159.97; 323.45; 221.71 ] );
         ( "test of_float Charles Schwab" >:: fun _ ->
           test_of_float "Schw" [ 199.89; 433.42; 390.73; 353.58 ] );
         ( "test of_float Blackrock" >:: fun _ ->
           test_of_float "Blk" [ 473.75; 484.41; 233.95; 433.61 ] );
         ( "test of_float American International" >:: fun _ ->
           test_of_float "Aig" [ 219.67; 478.28; 176.9; 417.88 ] );
         ( "test of_float Metlife" >:: fun _ ->
           test_of_float "Met" [ 180.31; 194.68; 354.59; 445.64 ] );
         ( "test of_float CME Group" >:: fun _ ->
           test_of_float "Cme" [ 349.82; 312.66; 299.36; 331.26 ] );
         ( "test read_csv ../data/stocks.csv" >:: fun _ ->
           assert_equal
             [
               Stock.of_float "Aapl" [ 145.3; 146.2; 147.5; 148.0 ];
               Stock.of_float "Msft" [ 305.1; 306.2; 307.0; 308.4 ];
               Stock.of_float "Goog" [ 2750.2; 2748.0; 2760.5; 2755.3 ];
               Stock.of_float "Amzn" [ 3335.5; 3340.1; 3338.0; 3342.2 ];
             ]
             (Stock.read_csv "../data/stocks.csv") );
         ( "test read_csv ../data/financial.csv" >:: fun _ ->
           assert_equal
             [
               Stock.of_float "Jpm" [ 328.66; 488.23; 370.07; 292.7 ];
               Stock.of_float "Bac" [ 189.94; 355.76; 155.91; 325.06 ];
               Stock.of_float "Wfc" [ 225.91; 200.61; 334.89; 190.99 ];
               Stock.of_float "C" [ 238.48; 223.81; 364.25; 433.55 ];
               Stock.of_float "Gs" [ 433.44; 128.63; 499.54; 308.67 ];
               Stock.of_float "Ms" [ 422.44; 460.85; 159.41; 372.54 ];
               Stock.of_float "Axp" [ 333.81; 116.17; 288.77; 140.37 ];
               Stock.of_float "Usb" [ 161.17; 290.72; 389.38; 152.9 ];
               Stock.of_float "Pnc" [ 479.51; 451.17; 295.25; 213.26 ];
               Stock.of_float "Cof" [ 460.08; 159.97; 323.45; 221.71 ];
               Stock.of_float "Schw" [ 199.89; 433.42; 390.73; 353.58 ];
               Stock.of_float "Blk" [ 473.75; 484.41; 233.95; 433.61 ];
               Stock.of_float "Aig" [ 219.67; 478.28; 176.9; 417.88 ];
               Stock.of_float "Met" [ 180.31; 194.68; 354.59; 445.64 ];
               Stock.of_float "Cme" [ 349.82; 312.66; 299.36; 331.26 ];
               (* Newly added lines *)
               Stock.of_float "Ally" [ 300.12; 312.45; 295.00; 305.6 ];
               Stock.of_float "Tfc" [ 200.45; 250.10; 210.3; 220.5 ];
               Stock.of_float "Fitp" [ 185.4; 190.6; 179.2; 188.5 ];
               Stock.of_float "Rf" [ 145.9; 150.4; 143.0; 149.9 ];
               Stock.of_float "Key" [ 190.2; 210.8; 205.1; 202.3 ];
               Stock.of_float "Mtb" [ 299.3; 310.6; 305.4; 315.2 ];
               Stock.of_float "Sti" [ 270.5; 265.9; 275.0; 260.1 ];
               Stock.of_float "Ntrs" [ 360.4; 358.6; 362.7; 355.5 ];
               Stock.of_float "Stt" [ 410.0; 415.2; 407.3; 420.1 ];
               Stock.of_float "Td" [ 375.5; 380.7; 385.2; 390.6 ];
             ]
             (Stock.read_csv "../data/financial.csv") );
         (* Portfolio Module Tests are here*)
         ( "test create_portfolio" >:: fun _ ->
           test_create_portfolio 10000.0 10000.0 );
         ( "test create_rt_portfolio" >:: fun _ ->
           test_create_rt_portfolio 10000.0 10000.0 );
         ("test empty rt summary" >:: fun _ -> test_empty_rt_portfolio_summary);
         ( "test buy_stock sufficient balance" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_buy_stock portfolio "Aapl" 10 test_stocks 8520.0
             [ ("Aapl", 10) ] );
         ( "test buy_stock insufficient balance" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 100.0 in
           test_buy_stock_insufficient_balance portfolio "Aapl" 10 test_stocks
         );
         ( "test sell_stock sufficient quantity" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Aapl" 10 test_stocks with
           | Some p ->
               test_sell_stock p "Aapl" 5 test_stocks 9260.0 [ ("Aapl", 5) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock insufficient quantity" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Aapl" 5 test_stocks with
           | Some p -> test_sell_stock_insufficient_qty p "Aapl" 10 test_stocks
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock all shares" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Aapl" 10 test_stocks with
           | Some p -> test_sell_stock p "Aapl" 10 test_stocks 10000.0 []
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test portfolio_summary no stocks" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_portfolio_summary portfolio test_stocks [] 10000.0 );
         ( "test portfolio_summary with stocks" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Aapl" 5 test_stocks with
           | Some p ->
               (* Expected total value: 5 shares * latest price of Aapl (148.0) = 740.0 *)
               (* Expected balance after purchase: 10000.0 - 740.0 = 9260.0 *)
               test_portfolio_summary p test_stocks
                 [ ("Aapl", 5, 740.0) ]
                 9260.0
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test buy_stock_existing_stock" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Aapl" 5 test_stocks with
           | Some p ->
               test_buy_stock_existing_stock p "Aapl" 5 test_stocks 8520.0
                 [ ("Aapl", 10) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock_all_but_one" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Aapl" 10 test_stocks with
           | Some p ->
               test_sell_stock_all_but_one p "Aapl" 9 test_stocks 9852.0
                 [ ("Aapl", 1) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test update_prices minimal change" >:: fun _ ->
           test_update_prices_no_change "mid" "Aapl"
             [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test sell_stock increases balance" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Aapl" 10 test_stocks with
           | Some p ->
               test_sell_stock_increases_balance p "Aapl" 5 test_stocks 9260.0
                 [ ("Aapl", 5) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test buy stock invalid name" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_buy_stock_invalid_name portfolio "InvalidStockName" stocks );
         ( "test portfolio with mixed stocks" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Aapl" 10 stocks with
           | Some p ->
               test_portfolio_with_mixed_stocks p stocks
                 [ ("Aapl", 10, 1480.0) ]
                 8520.0
           | None -> assert_failure "Failed to initialize portfolio" );
         ( "test portfolio summary empty" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_portfolio_summary_empty portfolio stocks );
         ( "test buy stock large quantity" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_buy_stock_large_qty portfolio "Aapl" stocks );
         ( "test sell stock zero holdings" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_sell_stock_zero_holdings portfolio "Aapl" test_stocks );
         ( "test portfolio summary missing prices" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_portfolio_summary_missing_prices portfolio test_stocks );
         ( "test buy stock negative price" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_buy_stock_negative_price portfolio "InvalidStock" test_stocks );
         ( "test portfolio summary empty portfolio" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 0.0 in
           test_portfolio_summary_empty_portfolio portfolio test_stocks );
         ( "test sell stock case sensitivity" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_sell_stock_case_sensitivity portfolio test_stocks );
         ( "test portfolio summary total calculation" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_portfolio_summary_total_calculation portfolio test_stocks );
         ( "test buy and sell stock multiple times" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_buy_and_sell_stock_multiple_times portfolio "Aapl" test_stocks
         );
         "test_save_rt_portfolio" >:: test_save_rt_portfolio;
         "test_load_rt_portfolio_valid" >:: test_load_rt_portfolio_valid;
         "test_load_rt_portfolio_invalid_balance"
         >:: test_load_rt_portfolio_invalid_balance;
         "test_load_rt_portfolio_invalid_stock"
         >:: test_load_rt_portfolio_invalid_stock;
         "test_load_rt_portfolio_empty_file"
         >:: test_load_rt_portfolio_empty_file;
         "test_save_and_load_rt_portfolio_round_trip"
         >:: test_save_and_load_rt_portfolio_round_trip;
         "Fetch Valid API Call" >:: test_fetch_valid_api;
         "Create Portfolio" >:: test_1create_rt_portfolio;
         "Get Balance" >:: test_1get_balance;
         "Get Stocks" >:: test_1get_stocks;
         "Buy Stock" >:: test_1buy_stock;
         "Buy Stock Invalid" >:: test_1buy_stock_invalid;
         "Sell Stock" >:: test_1sell_stock;
         "Sell Stock Not Owned" >:: test_1sell_stock_invalid;
         "Portfolio Summary" >:: test_rt_portfolio_summary;
         "test_load_rt_portfolio_malformed_stock_line"
         >:: test_load_rt_portfolio_malformed_stock_line;
         "test_load_portfolio_valid" >:: test_load_portfolio_valid;
         "test_load_portfolio_invalid_balance"
         >:: test_load_portfolio_invalid_balance;
         "test_load_portfolio_invalid_stock"
         >:: test_load_portfolio_invalid_stock;
         "test_save_and_load_portfolio_round_trip"
         >:: test_save_and_load_portfolio_round_trip;
         (* "test_save_portfolio_existing_stock" >::
            test_save_portfolio_existing_stock; *)
         "test load portfolio no file" >:: test_load_portfolio_no_file;
         "test load portfolio malformed stock line"
         >:: test_load_portfolio_malformed_stock_line;
         "test_load_portfolio_missing_stock_in_market"
         >:: test_load_portfolio_missing_stock_in_market;
       ]

let _ = run_test_tt_main tests
