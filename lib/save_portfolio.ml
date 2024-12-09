open Stock
open Portfolio

let save_portfolio portfolio market filename =
  (* Fetch the balance and stocks *)
  let balance = get_balance portfolio in
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
  close_out oc

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
    let portfolio = create_portfolio 9999999999999999999. in
    update_balance
      (* keeps the balance the same instead of trying to actually buy the
         stocks*)
      (List.fold_left
         (fun acc (name, qty) ->
           match buy_stock acc name qty market with
           | Some updated_portfolio -> updated_portfolio
           | None -> acc)
         portfolio stocks)
      balance
    |> Option.some
  with
  | Sys_error _ -> None
  | Failure msg -> None
