let parse_arguments args =
  let rec aux args methods step window_size =
    match args with
    | ("-m" | "--method") :: method_name :: rest ->
      aux rest (method_name :: methods) step window_size
    | ("-s" | "--step") :: step_value :: rest ->
      (try
         let step = float_of_string step_value in
         aux rest methods step window_size
       with
       | Failure _ -> failwith "Invalid step value. It must be a float.")
    | ("-w" | "--window") :: window_size_value :: rest ->
      (try
         let window_size = int_of_string window_size_value in
         aux rest methods step window_size
       with
       | Failure _ -> failwith "Invalid window size. It must be an integer.")
    | [] -> List.rev methods, step, window_size
    | _ -> failwith "Unknown argument or missing value."
  in
  aux args [] 1.0 3
;;

let filter_methods methods =
  let available_methods = [ "linear"; "lagrange" ] in
  let filtered_methods =
    methods
    |> List.sort_uniq Stdlib.compare
    |> List.filter (fun method_name -> List.mem method_name available_methods)
  in
  match filtered_methods with
  | [] -> failwith "No valid methods specified"
  | methods -> methods
;;

let print_memory_usage () =
  let stat = Gc.stat () in
  let words_to_megabytes words =
    float_of_int words *. float_of_int (Sys.word_size / 8) /. 1_000_000.0
  in
  let heap_size_mb = words_to_megabytes stat.Gc.heap_words in
  Printf.printf "Memory used: %.2f MB\n" heap_size_mb
;;

let () =
  (* Remove Sys.argv.(0) (program name) *)
  let args = Array.to_list Sys.argv |> List.tl in
  let methods, step, window_size =
    try parse_arguments args with
    | Failure message ->
      Printf.eprintf "Error: %s\n" message;
      print_string
        "Usage: program -m <method1> -m <method2> ... -s <step> -w <window_size>\n";
      exit 1
  in
  let filtered_methods =
    try filter_methods methods with
    | Failure message ->
      Printf.eprintf "Error: %s\n" message;
      exit 1
  in
  Run.run_interpolation filtered_methods step window_size;
  print_memory_usage ()
;;
