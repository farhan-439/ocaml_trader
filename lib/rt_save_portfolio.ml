open Stock
open Rt_portfolio

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
  None (*also unimplemented*)

let serialize_rt_portfolio portfolio = failwith "unimplemented"
let deserialize_rt_portfolio data = failwith "unimplemented"
