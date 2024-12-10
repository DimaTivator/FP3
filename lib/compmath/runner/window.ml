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

let add_prefix_padding sequence padding_length =
  Seq.append (Seq.take padding_length (Seq.repeat None)) sequence
;;
