open Stock
open Rt_portfolio
open Lwt.Infix

let save_rt_portfolio (portfolio : Rt_portfolio.rt_portfolio)
    (filename : string) : unit =
  let summary, balance =
    Lwt_main.run (Rt_portfolio.rt_portfolio_summary portfolio)
  in
  let data = [ Printf.sprintf "balance,%.2f" balance ] in
  let rec add_stock_data acc stocks =
    match stocks with
    | [] -> acc
    | (name, qty, value) :: t ->
        add_stock_data (acc @ [ Printf.sprintf "%s,%d" name qty ]) t
  in
  print_endline "Here's what you're saving: \n";
  add_stock_data data summary |> List.iter print_endline;
  let csv_data = String.concat "\n" (add_stock_data data summary) in
  let oc = open_out filename in
  output_string oc csv_data;
  close_out oc;
  Printf.printf "Portfolio saved to %s\n" filename

let load_rt_portfolio (filename : string) : Rt_portfolio.rt_portfolio option =
  try
    (*get all the stocks from the file*)
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
    print_endline "\nLoaded portfolio data below: \n";
    List.iter print_endline lines;
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
          | [ name; qty ] -> (name, int_of_string qty)
          | _ -> failwith "Malformed stock line in CSV file")
        stock_lines
    in
    let empty_portfolio = create_rt_portfolio 999999999999999999999. in
    (*large number to make sure rebuying all stocks is possible - bit of a hack
      but it's okay, similar to the simulated alternative*)
    let rec purchase_stocks (pf : rt_portfolio option) lst =
      match lst with
      | [] -> pf
      | (stock, qty) :: t -> (
          match pf with
          | None -> pf
          | Some portfolio ->
              purchase_stocks (Lwt_main.run (buy_stock portfolio stock qty)) t)
    in
    let full_portfolio = purchase_stocks (Some empty_portfolio) stocks in
    match full_portfolio with
    | Some x -> Some (update_rt_balance x balance)
    | None -> None
  with _ -> None

let serialize_rt_portfolio portfolio = failwith "unimplemented"
let deserialize_rt_portfolio data = failwith "unimplemented"
