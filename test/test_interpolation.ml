open Compmath

let test_linear_interpolation () =
  let points = [ 0., 0.; 2., 2. ] in
  let interpolate = Interpolation.linear_interpolation points in
  Alcotest.(check (float 0.001)) "linear interpolation at 1.0" 1.0 (interpolate 1.0);
  Alcotest.(check (float 0.001)) "linear interpolation at 0.5" 0.5 (interpolate 0.5)
;;

let test_lagrange_interpolation () =
  let points = [ 0., 0.; 1., 1.; 2., 4. ] in
  (* y = x^2 *)
  let interpolate = Interpolation.lagrange_interpolation points in
  Alcotest.(check (float 0.001)) "lagrange interpolation at 1.5" 2.25 (interpolate 1.5)
;;

let interpolation_tests =
  [ "Linear interpolation", `Quick, test_linear_interpolation
  ; "Lagrange interpolation", `Quick, test_lagrange_interpolation
  ]
;;
