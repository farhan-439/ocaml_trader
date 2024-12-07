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
  match Portfolio.sell_stock portfolio "APPLE" 10 market with
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

let test_stocks =
  [
    Stock.of_float "Apple" [ 145.3; 146.2; 147.5; 148.0 ];
    Stock.of_float "Microsoft" [ 305.1; 306.2; 307.0; 308.4 ];
    Stock.of_float "Google" [ 2750.2; 2748.0; 2760.5; 2755.3 ];
    Stock.of_float "Amazon" [ 3335.5; 3340.1; 3338.0; 3342.2 ];
  ]

let tests =
  "test suite"
  >::: [
         ("test api fetch good" >:: fun _ -> test_api_fetch_good "aapl");
         ("test api fetch bad" >:: fun _ -> test_api_fetch_bad "cs3110");
         ( "test dune functionality" >:: fun _ ->
           assert_equal 0 0 ~printer:string_of_int;
           assert_equal "1" "1" ~printer:Fun.id );
         ( "test get_prices Apple" >:: fun _ ->
           test_get_prices "Apple"
             [ 145.3; 146.2; 147.5; 148.0 ]
             "../data/stocks.csv" );
         ( "test get_prices Microsoft" >:: fun _ ->
           test_get_prices "Microsoft"
             [ 305.1; 306.2; 307.0; 308.4 ]
             "../data/stocks.csv" );
         ( "test get_prices Google" >:: fun _ ->
           test_get_prices "Google"
             [ 2750.2; 2748.0; 2760.5; 2755.3 ]
             "../data/stocks.csv" );
         ( "test get_prices Amazon" >:: fun _ ->
           test_get_prices "Amazon"
             [ 3335.5; 3340.1; 3338.0; 3342.2 ]
             "../data/stocks.csv" );
         ( "test get_prices JP Morgan" >:: fun _ ->
           test_get_prices "Jp morgan"
             [ 328.66; 488.23; 370.07; 292.7 ]
             "../data/financial.csv" );
         ( "test get_prices Bank of America" >:: fun _ ->
           test_get_prices "Bank of america"
             [ 189.94; 355.76; 155.91; 325.06 ]
             "../data/financial.csv" );
         ( "test get_prices Wells Fargo" >:: fun _ ->
           test_get_prices "Wells fargo"
             [ 225.91; 200.61; 334.89; 190.99 ]
             "../data/financial.csv" );
         ( "test get_prices Citi Group" >:: fun _ ->
           test_get_prices "Citi group"
             [ 238.48; 223.81; 364.25; 433.55 ]
             "../data/financial.csv" );
         ( "test get_prices Goldman Sachs" >:: fun _ ->
           test_get_prices "Goldman sachs"
             [ 433.44; 128.63; 499.54; 308.67 ]
             "../data/financial.csv" );
         ( "test get_prices Morgan Stanley" >:: fun _ ->
           test_get_prices "Morgan stanley"
             [ 422.44; 460.85; 159.41; 372.54 ]
             "../data/financial.csv" );
         ( "test get_prices American Express" >:: fun _ ->
           test_get_prices "American express"
             [ 333.81; 116.17; 288.77; 140.37 ]
             "../data/financial.csv" );
         ( "test get_prices Ally Financial" >:: fun _ ->
           test_get_prices "Ally financial"
             [ 300.12; 312.45; 295.00; 305.6 ]
             "../data/financial.csv" );
         ( "test get_prices BB&T Corp" >:: fun _ ->
           test_get_prices "Bb&t corp"
             [ 200.45; 250.10; 210.3; 220.5 ]
             "../data/financial.csv" );
         ( "test get_prices Fifth Third Bancorp" >:: fun _ ->
           test_get_prices "Fifth third bancorp"
             [ 185.4; 190.6; 179.2; 188.5 ]
             "../data/financial.csv" );
         ( "test get_prices Regions Financial" >:: fun _ ->
           test_get_prices "Regions financial"
             [ 145.9; 150.4; 143.0; 149.9 ]
             "../data/financial.csv" );
         ( "test get_prices KeyCorp" >:: fun _ ->
           test_get_prices "Keycorp"
             [ 190.2; 210.8; 205.1; 202.3 ]
             "../data/financial.csv" );
         ( "test get_prices M&T Bank" >:: fun _ ->
           test_get_prices "M&t bank"
             [ 299.3; 310.6; 305.4; 315.2 ]
             "../data/financial.csv" );
         ( "test get_prices SunTrust Banks" >:: fun _ ->
           test_get_prices "Suntrust banks"
             [ 270.5; 265.9; 275.0; 260.1 ]
             "../data/financial.csv" );
         ( "test get_prices Northern Trust" >:: fun _ ->
           test_get_prices "Northern trust"
             [ 360.4; 358.6; 362.7; 355.5 ]
             "../data/financial.csv" );
         ( "test get_prices State Street" >:: fun _ ->
           test_get_prices "State street"
             [ 410.0; 415.2; 407.3; 420.1 ]
             "../data/financial.csv" );
         ( "test get_prices TD Bank" >:: fun _ ->
           test_get_prices "Td bank"
             [ 375.5; 380.7; 385.2; 390.6 ]
             "../data/financial.csv" );
         (* update_prices tests for new entries (varying patterns) *)
         ( "test update_prices high Ally Financial" >:: fun _ ->
           test_update_prices "high" "Ally financial"
             [ 300.12; 312.45; 295.00; 305.6 ] );
         ( "test update_prices mid BB&T Corp" >:: fun _ ->
           test_update_prices "mid" "Bb&t corp" [ 200.45; 250.10; 210.3; 220.5 ]
         );
         ( "test update_prices low Fifth Third Bancorp" >:: fun _ ->
           test_update_prices "low" "Fifth third bancorp"
             [ 185.4; 190.6; 179.2; 188.5 ] );
         ( "test update_prices high Regions Financial" >:: fun _ ->
           test_update_prices "high" "Regions financial"
             [ 145.9; 150.4; 143.0; 149.9 ] );
         ( "test update_prices mid KeyCorp" >:: fun _ ->
           test_update_prices "mid" "Keycorp" [ 190.2; 210.8; 205.1; 202.3 ] );
         ( "test update_prices low M&T Bank" >:: fun _ ->
           test_update_prices "low" "M&t bank" [ 299.3; 310.6; 305.4; 315.2 ] );
         ( "test update_prices high SunTrust Banks" >:: fun _ ->
           test_update_prices "high" "Suntrust banks"
             [ 270.5; 265.9; 275.0; 260.1 ] );
         ( "test update_prices mid Northern Trust" >:: fun _ ->
           test_update_prices "mid" "Northern trust"
             [ 360.4; 358.6; 362.7; 355.5 ] );
         ( "test update_prices low State Street" >:: fun _ ->
           test_update_prices "low" "State street"
             [ 410.0; 415.2; 407.3; 420.1 ] );
         ( "test update_prices high TD Bank" >:: fun _ ->
           test_update_prices "high" "Td bank" [ 375.5; 380.7; 385.2; 390.6 ] );
         (* to_float tests for new entries *)
         ( "test to_float Ally Financial" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 15)
             "Ally financial"
             [ 300.12; 312.45; 295.00; 305.6 ] );
         ( "test to_float BB&T Corp" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 16)
             "Bb&t corp"
             [ 200.45; 250.10; 210.3; 220.5 ] );
         ( "test to_float Fifth Third Bancorp" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 17)
             "Fifth third bancorp"
             [ 185.4; 190.6; 179.2; 188.5 ] );
         ( "test to_float Regions Financial" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 18)
             "Regions financial"
             [ 145.9; 150.4; 143.0; 149.9 ] );
         ( "test to_float KeyCorp" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 19)
             "Keycorp"
             [ 190.2; 210.8; 205.1; 202.3 ] );
         ( "test to_float M&T Bank" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 20)
             "M&t bank"
             [ 299.3; 310.6; 305.4; 315.2 ] );
         ( "test to_float SunTrust Banks" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 21)
             "Suntrust banks"
             [ 270.5; 265.9; 275.0; 260.1 ] );
         ( "test to_float Northern Trust" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 22)
             "Northern trust"
             [ 360.4; 358.6; 362.7; 355.5 ] );
         ( "test to_float State Street" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 23)
             "State street"
             [ 410.0; 415.2; 407.3; 420.1 ] );
         ( "test to_float TD Bank" >:: fun _ ->
           test_to_float
             (List.nth (Stock.read_csv "../data/financial.csv") 24)
             "Td bank"
             [ 375.5; 380.7; 385.2; 390.6 ] );
         (* of_float tests for new entries *)
         ( "test of_float Ally Financial" >:: fun _ ->
           test_of_float "Ally financial" [ 300.12; 312.45; 295.00; 305.6 ] );
         ( "test of_float BB&T Corp" >:: fun _ ->
           test_of_float "Bb&t corp" [ 200.45; 250.10; 210.3; 220.5 ] );
         ( "test of_float Fifth Third Bancorp" >:: fun _ ->
           test_of_float "Fifth third bancorp" [ 185.4; 190.6; 179.2; 188.5 ] );
         ( "test of_float Regions Financial" >:: fun _ ->
           test_of_float "Regions financial" [ 145.9; 150.4; 143.0; 149.9 ] );
         ( "test of_float KeyCorp" >:: fun _ ->
           test_of_float "Keycorp" [ 190.2; 210.8; 205.1; 202.3 ] );
         ( "test of_float M&T Bank" >:: fun _ ->
           test_of_float "M&t bank" [ 299.3; 310.6; 305.4; 315.2 ] );
         ( "test of_float SunTrust Banks" >:: fun _ ->
           test_of_float "Suntrust banks" [ 270.5; 265.9; 275.0; 260.1 ] );
         ( "test of_float Northern Trust" >:: fun _ ->
           test_of_float "Northern trust" [ 360.4; 358.6; 362.7; 355.5 ] );
         ( "test of_float State Street" >:: fun _ ->
           test_of_float "State street" [ 410.0; 415.2; 407.3; 420.1 ] );
         ( "test of_float TD Bank" >:: fun _ ->
           test_of_float "Td bank" [ 375.5; 380.7; 385.2; 390.6 ] );
         ( "test get_prices US Bancorp" >:: fun _ ->
           test_get_prices "US bancorp"
             [ 161.17; 290.72; 389.38; 152.9 ]
             "../data/financial.csv" );
         ( "test get_prices PNC Financial" >:: fun _ ->
           test_get_prices "Pnc financial"
             [ 479.51; 451.17; 295.25; 213.26 ]
             "../data/financial.csv" );
         ( "test get_prices Capital One" >:: fun _ ->
           test_get_prices "Capital one"
             [ 460.08; 159.97; 323.45; 221.71 ]
             "../data/financial.csv" );
         ( "test get_prices Charles Schwab" >:: fun _ ->
           test_get_prices "Charles schwab"
             [ 199.89; 433.42; 390.73; 353.58 ]
             "../data/financial.csv" );
         ( "test get_prices BlackRock" >:: fun _ ->
           test_get_prices "Blackrock"
             [ 473.75; 484.41; 233.95; 433.61 ]
             "../data/financial.csv" );
         ( "test get_prices American International" >:: fun _ ->
           test_get_prices "American international"
             [ 219.67; 478.28; 176.9; 417.88 ]
             "../data/financial.csv" );
         ( "test get_prices MetLife" >:: fun _ ->
           test_get_prices "Metlife"
             [ 180.31; 194.68; 354.59; 445.64 ]
             "../data/financial.csv" );
         ( "test get_prices CME Group" >:: fun _ ->
           test_get_prices "Cme group"
             [ 349.82; 312.66; 299.36; 331.26 ]
             "../data/financial.csv" );
         ( "test update_prices high Apple" >:: fun _ ->
           test_update_prices "high" "Apple" [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test update_prices mid Microsoft" >:: fun _ ->
           test_update_prices "mid" "Microsoft" [ 305.1; 306.2; 307.0; 308.4 ]
         );
         ( "test update_prices low Google" >:: fun _ ->
           test_update_prices "low" "Google" [ 2750.2; 2748.0; 2760.5; 2755.3 ]
         );
         ( "test update_prices high Amazon" >:: fun _ ->
           test_update_prices "high" "Amazon" [ 3335.5; 3340.1; 3338.0; 3342.2 ]
         );
         ( "test update_prices high JP Morgan" >:: fun _ ->
           test_update_prices "high" "Jp morgan"
             [ 328.66; 488.23; 370.07; 292.7 ] );
         ( "test update_prices mid Bank of America" >:: fun _ ->
           test_update_prices "mid" "Bank of america"
             [ 189.94; 355.76; 155.91; 325.06 ] );
         ( "test update_prices low Wells Fargo" >:: fun _ ->
           test_update_prices "low" "Wells fargo"
             [ 225.91; 200.61; 334.89; 190.99 ] );
         ( "test update_prices high Citi Group" >:: fun _ ->
           test_update_prices "high" "Citi group"
             [ 238.48; 223.81; 364.25; 433.55 ] );
         ( "test update_prices mid Goldman Sachs" >:: fun _ ->
           test_update_prices "mid" "Goldman sachs"
             [ 433.44; 128.63; 499.54; 308.67 ] );
         ( "test update_prices low Morgan Stanley" >:: fun _ ->
           test_update_prices "low" "Morgan stanley"
             [ 422.44; 460.85; 159.41; 372.54 ] );
         ( "test update_prices high American Express" >:: fun _ ->
           test_update_prices "high" "American express"
             [ 333.81; 116.17; 288.77; 140.37 ] );
         ( "test update_prices mid US Bancorp" >:: fun _ ->
           test_update_prices "mid" "Us bancorp"
             [ 161.17; 290.72; 389.38; 152.9 ] );
         ( "test update_prices low PNC financial" >:: fun _ ->
           test_update_prices "low" "Pnc financial"
             [ 479.51; 451.17; 295.25; 213.26 ] );
         ( "test update_prices high Capital One" >:: fun _ ->
           test_update_prices "high" "Capital one"
             [ 460.08; 159.97; 323.45; 221.71 ] );
         ( "test update_prices mid Charles Schwab" >:: fun _ ->
           test_update_prices "mid" "Charles schwab"
             [ 199.89; 433.42; 390.73; 353.58 ] );
         ( "test update_prices low Blackrock" >:: fun _ ->
           test_update_prices "low" "Blackrock"
             [ 473.75; 484.41; 233.95; 433.61 ] );
         ( "test update_prices high American International" >:: fun _ ->
           test_update_prices "high" "American international"
             [ 219.67; 478.28; 176.9; 417.88 ] );
         ( "test update_prices mid Metlife" >:: fun _ ->
           test_update_prices "mid" "Metlife" [ 180.31; 194.68; 354.59; 445.64 ]
         );
         ( "test update_prices low CME Group" >:: fun _ ->
           test_update_prices "low" "Cme group"
             [ 349.82; 312.66; 299.36; 331.26 ] );
         ("test update_balance" >:: fun _ -> test_update_balance 100000. 100000.);
         ( "test update_rt_balance" >:: fun _ ->
           test_rt_update_balance 100000. 100000. );
         ( "test to_float Apple" >:: fun _ ->
           test_to_float (List.hd stocks) "Apple" [ 145.3; 146.2; 147.5; 148.0 ]
         );
         ( "test to_float Microsoft" >:: fun _ ->
           test_to_float (List.nth stocks 1) "Microsoft"
             [ 305.1; 306.2; 307.0; 308.4 ] );
         ( "test to_float Google" >:: fun _ ->
           test_to_float (List.nth stocks 2) "Google"
             [ 2750.2; 2748.0; 2760.5; 2755.3 ] );
         ( "test to_float Amazon" >:: fun _ ->
           test_to_float (List.nth stocks 3) "Amazon"
             [ 3335.5; 3340.1; 3338.0; 3342.2 ] );
         ( "test to_float JP Morgan" >:: fun _ ->
           test_to_float (List.hd fin_stocks) "Jp morgan"
             [ 328.66; 488.23; 370.07; 292.7 ] );
         ( "test to_float Bank of America" >:: fun _ ->
           test_to_float (List.nth fin_stocks 1) "Bank of america"
             [ 189.94; 355.76; 155.91; 325.06 ] );
         ( "test to_float Wells Fargo" >:: fun _ ->
           test_to_float (List.nth fin_stocks 2) "Wells fargo"
             [ 225.91; 200.61; 334.89; 190.99 ] );
         ( "test to_float Citi Group" >:: fun _ ->
           test_to_float (List.nth fin_stocks 3) "Citi group"
             [ 238.48; 223.81; 364.25; 433.55 ] );
         ( "test to_float Goldman Sachs" >:: fun _ ->
           test_to_float (List.nth fin_stocks 4) "Goldman sachs"
             [ 433.44; 128.63; 499.54; 308.67 ] );
         ( "test to_float Morgan Stanley" >:: fun _ ->
           test_to_float (List.nth fin_stocks 5) "Morgan stanley"
             [ 422.44; 460.85; 159.41; 372.54 ] );
         ( "test to_float American Express" >:: fun _ ->
           test_to_float (List.nth fin_stocks 6) "American express"
             [ 333.81; 116.17; 288.77; 140.37 ] );
         ( "test to_float US Bancorp" >:: fun _ ->
           test_to_float (List.nth fin_stocks 7) "Us bancorp"
             [ 161.17; 290.72; 389.38; 152.9 ] );
         ( "test to_float PNC financial" >:: fun _ ->
           test_to_float (List.nth fin_stocks 8) "Pnc financial"
             [ 479.51; 451.17; 295.25; 213.26 ] );
         ( "test to_float Capital One" >:: fun _ ->
           test_to_float (List.nth fin_stocks 9) "Capital one"
             [ 460.08; 159.97; 323.45; 221.71 ] );
         ( "test to_float Charles Schwab" >:: fun _ ->
           test_to_float (List.nth fin_stocks 10) "Charles schwab"
             [ 199.89; 433.42; 390.73; 353.58 ] );
         ( "test to_float Blackrock" >:: fun _ ->
           test_to_float (List.nth fin_stocks 11) "Blackrock"
             [ 473.75; 484.41; 233.95; 433.61 ] );
         ( "test to_float American International" >:: fun _ ->
           test_to_float (List.nth fin_stocks 12) "American international"
             [ 219.67; 478.28; 176.9; 417.88 ] );
         ( "test to_float Metlife" >:: fun _ ->
           test_to_float (List.nth fin_stocks 13) "Metlife"
             [ 180.31; 194.68; 354.59; 445.64 ] );
         ( "test to_float CME Group" >:: fun _ ->
           test_to_float (List.nth fin_stocks 14) "Cme group"
             [ 349.82; 312.66; 299.36; 331.26 ] );
         ( "test of_float Apple" >:: fun _ ->
           test_of_float "Apple" [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test of_float Microsoft" >:: fun _ ->
           test_of_float "Microsoft" [ 305.1; 306.2; 307.0; 308.4 ] );
         ( "test of_float Google" >:: fun _ ->
           test_of_float "Google" [ 2750.2; 2748.0; 2760.5; 2755.3 ] );
         ( "test of_float Amazon" >:: fun _ ->
           test_of_float "Amazon" [ 3335.5; 3340.1; 3338.0; 3342.2 ] );
         ( "test of_float JP Morgan" >:: fun _ ->
           test_of_float "Jp morgan" [ 328.66; 488.23; 370.07; 292.7 ] );
         ( "test of_float Bank of America" >:: fun _ ->
           test_of_float "Bank of america" [ 189.94; 355.76; 155.91; 325.06 ] );
         ( "test of_float Wells Fargo" >:: fun _ ->
           test_of_float "Wells fargo" [ 225.91; 200.61; 334.89; 190.99 ] );
         ( "test of_float Citi Group" >:: fun _ ->
           test_of_float "Citi group" [ 238.48; 223.81; 364.25; 433.55 ] );
         ( "test of_float Goldman Sachs" >:: fun _ ->
           test_of_float "Goldman sachs" [ 433.44; 128.63; 499.54; 308.67 ] );
         ( "test of_float Morgan Stanley" >:: fun _ ->
           test_of_float "Morgan stanley" [ 422.44; 460.85; 159.41; 372.54 ] );
         ( "test of_float American Express" >:: fun _ ->
           test_of_float "American express" [ 333.81; 116.17; 288.77; 140.37 ]
         );
         ( "test of_float US Bancorp" >:: fun _ ->
           test_of_float "Us bancorp" [ 161.17; 290.72; 389.38; 152.9 ] );
         ( "test of_float PNC financial" >:: fun _ ->
           test_of_float "Pnc financial" [ 479.51; 451.17; 295.25; 213.26 ] );
         ( "test of_float Capital One" >:: fun _ ->
           test_of_float "Capital one" [ 460.08; 159.97; 323.45; 221.71 ] );
         ( "test of_float Charles Schwab" >:: fun _ ->
           test_of_float "Charles schwab" [ 199.89; 433.42; 390.73; 353.58 ] );
         ( "test of_float Blackrock" >:: fun _ ->
           test_of_float "Blackrock" [ 473.75; 484.41; 233.95; 433.61 ] );
         ( "test of_float American International" >:: fun _ ->
           test_of_float "American international"
             [ 219.67; 478.28; 176.9; 417.88 ] );
         ( "test of_float Metlife" >:: fun _ ->
           test_of_float "Metlife" [ 180.31; 194.68; 354.59; 445.64 ] );
         ( "test of_float CME Group" >:: fun _ ->
           test_of_float "Cme group" [ 349.82; 312.66; 299.36; 331.26 ] );
         ( "test read_csv ../data/stocks.csv" >:: fun _ ->
           assert_equal
             [
               Stock.of_float "Apple" [ 145.3; 146.2; 147.5; 148.0 ];
               Stock.of_float "Microsoft" [ 305.1; 306.2; 307.0; 308.4 ];
               Stock.of_float "Google" [ 2750.2; 2748.0; 2760.5; 2755.3 ];
               Stock.of_float "Amazon" [ 3335.5; 3340.1; 3338.0; 3342.2 ];
             ]
             (Stock.read_csv "../data/stocks.csv") );
         ( "test read_csv ../data/financial.csv" >:: fun _ ->
           assert_equal
             [
               Stock.of_float "Jp morgan" [ 328.66; 488.23; 370.07; 292.7 ];
               Stock.of_float "Bank of america"
                 [ 189.94; 355.76; 155.91; 325.06 ];
               Stock.of_float "Wells fargo" [ 225.91; 200.61; 334.89; 190.99 ];
               Stock.of_float "Citi group" [ 238.48; 223.81; 364.25; 433.55 ];
               Stock.of_float "Goldman sachs" [ 433.44; 128.63; 499.54; 308.67 ];
               Stock.of_float "Morgan stanley"
                 [ 422.44; 460.85; 159.41; 372.54 ];
               Stock.of_float "American express"
                 [ 333.81; 116.17; 288.77; 140.37 ];
               Stock.of_float "Us bancorp" [ 161.17; 290.72; 389.38; 152.9 ];
               Stock.of_float "Pnc financial" [ 479.51; 451.17; 295.25; 213.26 ];
               Stock.of_float "Capital one" [ 460.08; 159.97; 323.45; 221.71 ];
               Stock.of_float "Charles schwab"
                 [ 199.89; 433.42; 390.73; 353.58 ];
               Stock.of_float "Blackrock" [ 473.75; 484.41; 233.95; 433.61 ];
               Stock.of_float "American international"
                 [ 219.67; 478.28; 176.9; 417.88 ];
               Stock.of_float "Metlife" [ 180.31; 194.68; 354.59; 445.64 ];
               Stock.of_float "Cme group" [ 349.82; 312.66; 299.36; 331.26 ];
               (* Newly added lines *)
               Stock.of_float "Ally financial" [ 300.12; 312.45; 295.00; 305.6 ];
               Stock.of_float "Bb&t corp" [ 200.45; 250.10; 210.3; 220.5 ];
               Stock.of_float "Fifth third bancorp"
                 [ 185.4; 190.6; 179.2; 188.5 ];
               Stock.of_float "Regions financial" [ 145.9; 150.4; 143.0; 149.9 ];
               Stock.of_float "Keycorp" [ 190.2; 210.8; 205.1; 202.3 ];
               Stock.of_float "M&t bank" [ 299.3; 310.6; 305.4; 315.2 ];
               Stock.of_float "Suntrust banks" [ 270.5; 265.9; 275.0; 260.1 ];
               Stock.of_float "Northern trust" [ 360.4; 358.6; 362.7; 355.5 ];
               Stock.of_float "State street" [ 410.0; 415.2; 407.3; 420.1 ];
               Stock.of_float "Td bank" [ 375.5; 380.7; 385.2; 390.6 ];
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
           test_buy_stock portfolio "Apple" 10 test_stocks 8520.0
             [ ("Apple", 10) ] );
         ( "test buy_stock insufficient balance" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 100.0 in
           test_buy_stock_insufficient_balance portfolio "Apple" 10 test_stocks
         );
         ( "test sell_stock sufficient quantity" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Apple" 10 test_stocks with
           | Some p ->
               test_sell_stock p "Apple" 5 test_stocks 9260.0 [ ("Apple", 5) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock insufficient quantity" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Apple" 5 test_stocks with
           | Some p -> test_sell_stock_insufficient_qty p "Apple" 10 test_stocks
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock all shares" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Apple" 10 test_stocks with
           | Some p -> test_sell_stock p "Apple" 10 test_stocks 10000.0 []
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test portfolio_summary no stocks" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_portfolio_summary portfolio test_stocks [] 10000.0 );
         ( "test portfolio_summary with stocks" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Apple" 5 test_stocks with
           | Some p ->
               (* Expected total value: 5 shares * latest price of Apple (148.0) = 740.0 *)
               (* Expected balance after purchase: 10000.0 - 740.0 = 9260.0 *)
               test_portfolio_summary p test_stocks
                 [ ("Apple", 5, 740.0) ]
                 9260.0
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test buy_stock_existing_stock" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Apple" 5 test_stocks with
           | Some p ->
               test_buy_stock_existing_stock p "Apple" 5 test_stocks 8520.0
                 [ ("Apple", 10) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock_all_but_one" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Apple" 10 test_stocks with
           | Some p ->
               test_sell_stock_all_but_one p "Apple" 9 test_stocks 9852.0
                 [ ("Apple", 1) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test update_prices minimal change" >:: fun _ ->
           test_update_prices_no_change "mid" "Apple"
             [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test sell_stock increases balance" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Apple" 10 test_stocks with
           | Some p ->
               test_sell_stock_increases_balance p "Apple" 5 test_stocks 9260.0
                 [ ("Apple", 5) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test buy stock invalid name" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_buy_stock_invalid_name portfolio "InvalidStockName" stocks );
         ( "test portfolio with mixed stocks" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           match Portfolio.buy_stock portfolio "Apple" 10 stocks with
           | Some p ->
               test_portfolio_with_mixed_stocks p stocks
                 [ ("Apple", 10, 1480.0) ]
                 8520.0
           | None -> assert_failure "Failed to initialize portfolio" );
         ( "test portfolio summary empty" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_portfolio_summary_empty portfolio stocks );
         ( "test buy stock large quantity" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_buy_stock_large_qty portfolio "Apple" stocks );
         ( "test sell stock zero holdings" >:: fun _ ->
           let portfolio = Portfolio.create_portfolio 10000.0 in
           test_sell_stock_zero_holdings portfolio "Apple" test_stocks );
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
           test_buy_and_sell_stock_multiple_times portfolio "Apple" test_stocks
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
         "test_load_rt_portfolio_malformed_stock_line"
         >:: test_load_rt_portfolio_malformed_stock_line;
       ]

let _ = run_test_tt_main tests
