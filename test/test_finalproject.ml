open OUnit2
open Finalproject
open Portfolio

let stocks = Stock.read_csv "../data/stocks.csv"

(**[float_list_printer] is a helper printer for pretty float list printing.*)
let float_list_printer lst =
  "[" ^ String.concat "; " (List.map string_of_float lst) ^ "]"

(**[test_get_prices stock price] will generate a test case for
   [Stock.get_prices] using [stock] and [price].*)
let test_get_prices stock price =
  assert_equal price (Stock.get_prices stock stocks) ~printer:float_list_printer

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
  match pattern with
  | "high" ->
      assert (last_price >= avg_price *. 1. && last_price <= avg_price *. 1.6)
  | "mid" ->
      assert (last_price >= avg_price *. 0.8 && last_price <= avg_price *. 1.2)
  | _ ->
      assert (last_price >= avg_price *. 0.4 && last_price <= avg_price *. 1.)
(*added larger margin of error to account for floating point errors*)

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
  let portfolio = create_portfolio balance in
  assert_equal expected_balance (get_balance portfolio) ~printer:string_of_float;
  assert_equal [] (get_stocks portfolio)

(** [test_buy_stock portfolio stock_name qty market expected_balance expected_stocks]
    tests buying stocks by checking the updated balance and stock list. *)
let test_buy_stock portfolio stock_name qty market expected_balance
    expected_stocks =
  match buy_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert_equal expected_balance
        (get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (get_stocks updated_portfolio)
  | None -> assert_failure "Expected purchase to succeed"

(** [test_buy_stock_insufficient_balance portfolio stock_name qty market] tests
    buying stocks with insufficient balance, expecting it to fail. *)
let test_buy_stock_insufficient_balance portfolio stock_name qty market =
  match buy_stock portfolio stock_name qty market with
  | None -> () (* Expected failure due to insufficient balance *)
  | Some _ ->
      assert_failure "Expected purchase to fail due to insufficient balance"

(** [test_sell_stock portfolio stock_name qty market expected_balance expected_stocks]
    tests selling stocks by checking the updated balance and stock list. *)
let test_sell_stock portfolio stock_name qty market expected_balance
    expected_stocks =
  match sell_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert_equal expected_balance
        (get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (get_stocks updated_portfolio)
  | None -> assert_failure "Expected sale to succeed"

(** [test_sell_stock_insufficient_qty portfolio stock_name qty market] tests
    selling stocks with an insufficient quantity, expecting it to fail. *)
let test_sell_stock_insufficient_qty portfolio stock_name qty market =
  match sell_stock portfolio stock_name qty market with
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
  match buy_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert_equal expected_balance
        (get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (get_stocks updated_portfolio)
  | None -> assert_failure "Expected purchase to succeed"

(** [test_sell_stock_all_but_one portfolio stock_name qty market expected_balance expected_stocks]
    tests selling all but one share of a stock, ensuring the stock remains in
    the portfolio with the correct quantity and balance update. *)
let test_sell_stock_all_but_one portfolio stock_name qty market expected_balance
    expected_stocks =
  match sell_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert_equal expected_balance
        (get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (get_stocks updated_portfolio)
  | None -> assert_failure "Expected sale to succeed"

(** [test_sell_stock_increases_balance portfolio stock_name qty market expected_balance expected_stocks]
    tests that the portfolio balance increases after selling a stock. *)
let test_sell_stock_increases_balance portfolio stock_name qty market
    expected_balance expected_stocks =
  match sell_stock portfolio stock_name qty market with
  | Some updated_portfolio ->
      assert (get_balance updated_portfolio > get_balance portfolio);
      assert_equal expected_balance
        (get_balance updated_portfolio)
        ~printer:string_of_float;
      assert_equal expected_stocks (get_stocks updated_portfolio)
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
         ( "test basic" >:: fun _ ->
           assert_equal 0 0 ~printer:string_of_int;
           assert_equal "1" "1" ~printer:Fun.id );
         ( "test get_prices Apple" >:: fun _ ->
           test_get_prices "Apple" [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test get_prices Microsoft" >:: fun _ ->
           test_get_prices "Microsoft" [ 305.1; 306.2; 307.0; 308.4 ] );
         ( "test get_prices Google" >:: fun _ ->
           test_get_prices "Google" [ 2750.2; 2748.0; 2760.5; 2755.3 ] );
         ( "test get_prices Amazon" >:: fun _ ->
           test_get_prices "Amazon" [ 3335.5; 3340.1; 3338.0; 3342.2 ] );
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
         ( "test of_float Apple" >:: fun _ ->
           test_of_float "Apple" [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test of_float Microsoft" >:: fun _ ->
           test_of_float "Microsoft" [ 305.1; 306.2; 307.0; 308.4 ] );
         ( "test of_float Google" >:: fun _ ->
           test_of_float "Google" [ 2750.2; 2748.0; 2760.5; 2755.3 ] );
         ( "test of_float Amazon" >:: fun _ ->
           test_of_float "Amazon" [ 3335.5; 3340.1; 3338.0; 3342.2 ] );
         ( "test read_csv ../data/stocks.csv" >:: fun _ ->
           assert_equal
             [
               Stock.of_float "Apple" [ 145.3; 146.2; 147.5; 148.0 ];
               Stock.of_float "Microsoft" [ 305.1; 306.2; 307.0; 308.4 ];
               Stock.of_float "Google" [ 2750.2; 2748.0; 2760.5; 2755.3 ];
               Stock.of_float "Amazon" [ 3335.5; 3340.1; 3338.0; 3342.2 ];
             ]
             (Stock.read_csv "../data/stocks.csv") );
         (* Portfolio Module Tests are here*)
         ( "test create_portfolio" >:: fun _ ->
           test_create_portfolio 10000.0 10000.0 );
         ( "test buy_stock sufficient balance" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           test_buy_stock portfolio "Apple" 10 test_stocks 8520.0
             [ ("Apple", 10) ] );
         ( "test buy_stock insufficient balance" >:: fun _ ->
           let portfolio = create_portfolio 100.0 in
           test_buy_stock_insufficient_balance portfolio "Apple" 10 test_stocks
         );
         ( "test sell_stock sufficient quantity" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           match buy_stock portfolio "Apple" 10 test_stocks with
           | Some p ->
               test_sell_stock p "Apple" 5 test_stocks 9260.0 [ ("Apple", 5) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock insufficient quantity" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           match buy_stock portfolio "Apple" 5 test_stocks with
           | Some p -> test_sell_stock_insufficient_qty p "Apple" 10 test_stocks
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock all shares" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           match buy_stock portfolio "Apple" 10 test_stocks with
           | Some p -> test_sell_stock p "Apple" 10 test_stocks 10000.0 []
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test portfolio_summary no stocks" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           test_portfolio_summary portfolio test_stocks [] 10000.0 );
         ( "test portfolio_summary with stocks" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           match buy_stock portfolio "Apple" 5 test_stocks with
           | Some p ->
               (* Expected total value: 5 shares * latest price of Apple (148.0) = 740.0 *)
               (* Expected balance after purchase: 10000.0 - 740.0 = 9260.0 *)
               test_portfolio_summary p test_stocks
                 [ ("Apple", 5, 740.0) ]
                 9260.0
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test buy_stock_existing_stock" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           match buy_stock portfolio "Apple" 5 test_stocks with
           | Some p ->
               test_buy_stock_existing_stock p "Apple" 5 test_stocks 8520.0
                 [ ("Apple", 10) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test sell_stock_all_but_one" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           match buy_stock portfolio "Apple" 10 test_stocks with
           | Some p ->
               test_sell_stock_all_but_one p "Apple" 9 test_stocks 9852.0
                 [ ("Apple", 1) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
         ( "test update_prices minimal change" >:: fun _ ->
           test_update_prices_no_change "mid" "Apple"
             [ 145.3; 146.2; 147.5; 148.0 ] );
         ( "test sell_stock increases balance" >:: fun _ ->
           let portfolio = create_portfolio 10000.0 in
           match buy_stock portfolio "Apple" 10 test_stocks with
           | Some p ->
               test_sell_stock_increases_balance p "Apple" 5 test_stocks 9260.0
                 [ ("Apple", 5) ]
           | None -> assert_failure "Failed to buy initial stock for testing" );
       ]

let _ = run_test_tt_main tests
