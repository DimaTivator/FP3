let eps = 1e-8

let linspace (left : float) (right : float) (step : float) : float Seq.t =
  Seq.ints 0
  |> Seq.map (fun index -> left +. (step *. float_of_int index))
  |> Seq.take_while (fun x -> x < right +. eps)
;;
