let linear_interpolation = function
  | [ (x1, y1); (x2, y2) ] -> fun x -> y1 +. ((y2 -. y1) *. (x -. x1) /. (x2 -. x1))
  | _ -> failwith "Invalid number of points for linear interpolation"
;;

let lagrange_interpolation points =
  let num_points = List.length points in
  if num_points < 2 then failwith "Not enough points to interpolate";
  let basis i x =
    let xi, _ = List.nth points i in
    List.fold_left
      (fun acc (j, (xj, _)) -> if i = j then acc else acc *. (x -. xj) /. (xi -. xj))
      1.0
      (List.mapi (fun index p -> index, p) points)
  in
  fun x ->
    List.fold_left
      (fun acc (i, (_, yi)) -> acc +. (yi *. basis i x))
      0.0
      (List.mapi (fun index p -> index, p) points)
;;
