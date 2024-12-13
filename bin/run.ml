open Compmath
open Io

let available_interpolators lagrange_window_size =
  [ ( "linear"
    , Runner.compute_interpolation_result
        "Linear interpolation"
        Interpolation.linear_interpolation
    , 2 )
  ; ( "lagrange"
    , Runner.compute_interpolation_result
        "Lagrange interpolation"
        Interpolation.lagrange_interpolation
    , lagrange_window_size )
  ]
;;

let seq_head seq =
  match Seq.uncons seq with
  | Some (head, _) -> head
  | None -> -.infinity

let run_interpolation methods step window_size =
  let runnable_methods =
    List.filter
      (fun (name, _, _) -> List.mem name methods)
      (available_interpolators window_size)
  in
  let interpolation_results =
    Runner.run_interpolation_methods
      step
      runnable_methods
      Input.input_points_stream
  in
  let output_tables =
    Seq.map
      (fun (name, _, _, points) -> Output.print_table_vertically name points)
      interpolation_results
  in
  Seq.iter print_string output_tables
;;
