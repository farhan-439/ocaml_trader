open OUnit2
open Finalproject

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
       ]

let _ = run_test_tt_main tests
