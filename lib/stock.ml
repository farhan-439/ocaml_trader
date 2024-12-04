type t = string * float list

let () = Random.self_init ()

let rec get_prices (name : string) (lst : t list) =
  let name = String.lowercase_ascii name in
  match lst with
  | h :: t -> if fst h = name then snd h else get_prices name t
  | _ -> []

(*returns the average of a float list to help with update_prices*)
let avg_helper lst =
  List.fold_left ( +. ) 0. lst /. float_of_int (List.length lst)

let update_prices pattern stock =
  let rand = Random.float 20. in
  let mult =
    if pattern = "high" then 1.1 +. (rand /. 100.) (*1.1 -> 1.3 randomly*)
    else if pattern = "mid" then 0.9 +. (rand /. 100.) (*.9 -> 1.1 randomly*)
    else 0.5 +. (rand /. 50.)
    (*0.5 -> 0.9 randomly*)
  in
  let priceAvg = avg_helper (snd stock) in
  (fst stock, snd stock @ [ priceAvg *. mult ])

let to_float (stock : t) = (stock |> fst |> String.capitalize_ascii, snd stock)

let of_float (name : string) (lst : float list) : t =
  (name |> String.lowercase_ascii, lst)

(*creates a pair of name, float list from csv input*)
let rec csv_helper = function
  | h :: t ->
      let name = List.hd h in
      (name, List.map float_of_string (List.tl h)) :: csv_helper t
  | _ -> []

let rec read_csv filename = csv_helper (Csv.load filename)
