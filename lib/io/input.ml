open Seq

let nzip (sequence_of_lists : 'a Seq.t list) : 'a list Seq.t =
  List.fold_left
    (fun accumulated_sequence new_sequence ->
      Seq.map2
        (fun current_list new_element -> current_list @ [ new_element ])
        accumulated_sequence
        new_sequence)
    (Seq.map (fun x -> [ x ]) (List.hd sequence_of_lists))
    (List.tl sequence_of_lists)
;;

let sliding_window (window_size : int) (sequence : 'a Seq.t) : 'a list Seq.t =
  let indices = take window_size (ints 0) in
  let shifted_sequences = map (fun offset -> drop offset sequence) indices in
  let sequence_list = List.of_seq shifted_sequences in
  nzip sequence_list
;;

let add_prefix_points_padding sequence padding_length =
  Seq.append (Seq.take padding_length (Seq.repeat (-.infinity, -.infinity))) sequence
;;

let points_parser points =
  match
    points |> String.trim |> String.split_on_char ' ' |> List.map float_of_string_opt
  with
  | [ Some x; Some y ] -> Some (x, y)
  | _ -> None
;;

let rec parsed_data_dispenser parser =
  match read_line () with
  | "exit" -> None
  | line ->
    (try
       match parser line with
       | None ->
         print_string "Invalid input format\n";
         parsed_data_dispenser parser
       | x -> x
     with
     | End_of_file -> None)
;;

let input_points_dispenser () = parsed_data_dispenser points_parser
let input_points_stream = of_dispenser input_points_dispenser

let shifted_points_stream =
  let points_stream = memoize input_points_stream in
  add_prefix_points_padding points_stream 1
;;

(* let last_two_points_stream =
   input_points_stream
   |> memoize
   |> map to_option_tuple
   |> (fun seq -> add_prefix_points_padding seq 1)
   |> sliding_window 2
   ;; *)
