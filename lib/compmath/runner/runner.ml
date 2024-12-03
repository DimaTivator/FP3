open Generator

let compute_interpolation_result method_name get_interpolation_function step points =
  let left = fst (List.hd points) in
  let right = fst (List.hd (List.rev points)) in
  let interpolation_function = get_interpolation_function points in
  let interpolated_points =
    map_points interpolation_function (linspace left right step)
  in
  method_name, interpolation_function, points, interpolated_points
;;

let process_sliding_windows step methods points =
  let min_window_size =
    methods
    |> List.map (fun (_, _, window_size) -> window_size)
    |> List.fold_left min max_int
  in
  let apply_method (_, func, window_size) =
    let windows = Window.sliding_window window_size points in
    let interpolated_values = Seq.map (fun values -> Some (func step values)) windows in
    Window.add_prefix_padding interpolated_values window_size min_window_size
  in
  List.map apply_method methods
;;

let run_interpolation_methods step methods points =
  points
  |> Seq.memoize
  |> process_sliding_windows step methods
  |> Window.nzip
  |> Seq.flat_map List.to_seq
  |> Seq.filter_map Fun.id
;;
