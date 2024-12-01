open Stock
open Portfolio

let serialize_portfolio portfolio market =
  let balance = portfolio_balance portfolio in
  let stocks = get_stocks portfolio in
  let stock_data =
    List.map
      (fun (name, qty) ->
        match
          List.find_opt (fun stock -> fst (to_float stock) = name) market
        with
        | Some stock ->
            let prices = get_prices name market in
            let current_value = float_of_int qty *. List.hd (List.rev prices) in
            Printf.sprintf "%s,%d,%.2f" name qty current_value
        | None -> Printf.sprintf "%s,%d,0.0" name qty)
      stocks
  in
  String.concat "\n" (Printf.sprintf "balance,%f" balance :: stock_data)

let deserialize_portfolio data market =
  let lines = String.split_on_char '\n' data in
  let balance_line = List.hd lines in
  let stock_lines = List.tl lines in
  let balance =
    match String.split_on_char ',' balance_line with
    | [ "balance"; b ] -> float_of_string b
    | _ -> failwith "Malformed balance line in portfolio CSV"
  in
  let stocks =
    List.filter_map
      (fun line ->
        match String.split_on_char ',' line with
        | [ name; qty; _ ] ->
            if List.exists (fun stock -> fst (to_float stock) = name) market
            then Some (name, int_of_string qty)
            else (
              Printf.printf "Warning: Stock not found: %s\n" name;
              None)
        | _ -> failwith "Malformed portfolio file")
      stock_lines
  in
  let portfolio = create_portfolio balance in
  List.fold_left
    (fun acc (name, qty) ->
      match buy_stock acc name qty market with
      | Some updated_portfolio -> updated_portfolio
      | None -> acc)
    portfolio stocks

let save_portfolio portfolio market filename =
  (* Fetch the balance and stocks *)
  let balance = portfolio_balance portfolio in
  let stocks = get_stocks portfolio in

  (* Construct the CSV data *)
  let stock_lines =
    List.map
      (fun (name, qty) ->
        match
          List.find_opt (fun stock -> fst (to_float stock) = name) market
        with
        | Some stock ->
            let prices = get_prices name market in
            let current_price = List.hd (List.rev prices) in
            Printf.sprintf "%s,%d,%.2f" name qty current_price
        | None -> Printf.sprintf "%s,%d,%.2f" name qty 0.0)
      stocks
  in
  let csv_data =
    String.concat "\n" (("balance," ^ string_of_float balance) :: stock_lines)
  in

  (* Write to the file *)
  let oc = open_out filename in
  output_string oc csv_data;
  close_out oc;
  Printf.printf "Portfolio saved to %s\n" filename

let load_portfolio filename market =
  try
    (* Read the entire file *)
    let lines =
      let ic = open_in filename in
      let rec read_lines acc =
        try read_lines (input_line ic :: acc)
        with End_of_file ->
          close_in ic;
          List.rev acc
      in
      read_lines []
    in

    (* Parse the balance line *)
    let balance =
      match String.split_on_char ',' (List.hd lines) with
      | [ "balance"; b ] -> float_of_string b
      | _ -> failwith "Malformed balance line in CSV file"
    in

    (* Parse the stock lines *)
    let stock_lines = List.tl lines in
    let stocks =
      List.map
        (fun line ->
          match String.split_on_char ',' line with
          | [ name; qty; _ ] -> (name, int_of_string qty)
          | _ -> failwith "Malformed stock line in CSV file")
        stock_lines
    in

    (* Create and return the portfolio *)
    let portfolio = create_portfolio balance in
    List.fold_left
      (fun acc (name, qty) ->
        match buy_stock acc name qty market with
        | Some updated_portfolio -> updated_portfolio
        | None -> acc)
      portfolio stocks
    |> Option.some
  with
  | Sys_error _ ->
      Printf.printf "No existing portfolio found.\n";
      None
  | Failure msg ->
      Printf.printf "Error loading portfolio: %s\n" msg;
      None
