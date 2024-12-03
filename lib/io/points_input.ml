open Seq

let points_parser points =
  match
    points |> String.trim |> String.split_on_char ' ' |> List.map float_of_string_opt
  with
  | [ Some x; Some y ] -> Some (x, y)
  | _ -> None
;;

let rec parsed_data_dispenser parser =
  try
    match read_line () |> parser with
    | None ->
      print_string "Invalid input format\n";
      parsed_data_dispenser parser
    | x -> x
  with
  | End_of_file -> None
;;

let input_points_dispenser () = parsed_data_dispenser points_parser
let input_points_stream = input_points_dispenser |> of_dispenser
