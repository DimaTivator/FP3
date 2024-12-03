open Compmath

(* Create a testable type for float pairs *)
let float_pair =
  let pp ppf (x, y) = Format.fprintf ppf "(%f, %f)" x y in
  let equal (x1, y1) (x2, y2) = Float.equal x1 x2 && Float.equal y1 y2 in
  Alcotest.testable pp equal
;;

let test_compute_interpolation_result () =
  let points = [ 0., 0.; 1., 1. ] in
  let name, _, orig_points, _ =
    Runner.compute_interpolation_result
      "test"
      Interpolation.linear_interpolation
      1.0
      points
  in
  Alcotest.(check string) "name matches" "test" name;
  Alcotest.(check (list float_pair)) "original points preserved" points orig_points
;;

let runner_tests =
  [ "Compute interpolation result", `Quick, test_compute_interpolation_result ]
;;
