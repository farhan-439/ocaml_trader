open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

type t = {
  ticker : string;
  price : float option;
}

(* Abstraction Function:
 *   t represents a stock with a ticker symbol and an optional price.
 *   - ticker is the stock's ticker symbol.
 *   - price is the current price of the stock, or None if the price is unavailable.
 *
 * Representation Invariant:
 *   - ticker is a non-empty string.
 *   - price, if present, is a non-negative float.
 *)


let fetch_stock_price ticker =
  let url =
    Printf.sprintf "https://finnhub.io/api/v1/quote?symbol=%s&token=%s" ticker
      "csof9ahr01qt3r34a9rgcsof9ahr01qt3r34a9s0"
  in
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let status = resp |> Response.status |> Cohttp.Code.code_of_status in
  if status = 200 then
    body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
    let json = Yojson.Basic.from_string body_str in
    let price =
      json |> Yojson.Basic.Util.member "c" |> Yojson.Basic.Util.to_float_option
    in
    match price with
    | Some p -> Some { ticker; price = Some p }
    | None -> None
  else Lwt.return_none

  