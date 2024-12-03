open Compmath
open Cmdliner
open Io

let run_interpolation step selected_methods lagrange_window_size =
  let available_interpolators =
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
  in
  let active_methods =
    List.filter
      (fun (method_name, _, _) -> List.mem method_name selected_methods)
      available_interpolators
  in
  let interpolation_results =
    Runner.run_interpolation_methods step active_methods Points_input.input_points_stream
  in
  let output_tables =
    Seq.map
      (fun (name, _, _, points) -> Output.print_table name points)
      interpolation_results
  in
  Seq.iter print_string output_tables
;;

let window =
  let doc = "Window size for Lagrange interpolation (default: 3)" in
  Arg.(value & opt int 3 & info [ "w"; "window" ] ~doc)
;;

let step = Arg.(value & opt float 1.0 & info [ "s"; "step" ])
let methods = [ "linear"; "lagrange" ]

let method_names =
  Arg.(
    value & opt_all (enum (List.map (fun m -> m, m) methods)) [] & info [ "m"; "method" ])
;;

let parse_cli_args step method_names lagrange_window_size =
  match method_names with
  | [] -> failwith "No interpolation method specified"
  | methods ->
    run_interpolation step (List.sort_uniq Stdlib.compare methods) lagrange_window_size
;;

let cli = Term.(const parse_cli_args $ step $ method_names $ window)
let command = Cmd.v (Cmd.info "Interpolation app") cli
let () = exit (Cmd.eval command)
