type t = float list

let get_prices name = failwith "Unimplemented"
let update_prices pattern stock = stock
let to_float stock = [ 1. ]
let of_float lst = lst

let rec csv_helper = function
  | h :: t -> List.map float_of_string h :: csv_helper t
  | _ -> []

let rec read_csv filename = csv_helper (Csv.load filename)
