open Seq

(** [nzip] takes a list of sequences and "zips" them together into a sequence of lists.
    Each element in the output sequence contains corresponding elements from all input sequences.

    Example:
    Let's say we have two sequences: [1; 2; 3] and ['a'; 'b'; 'c']
    nzip [seq[1; 2; 3]; seq['a'; 'b'; 'c']] will produce:
    seq[[1; 'a']; [2; 'b']; [3; 'c']] *)
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

(** [sliding_window window_size sequence] creates a sequence of overlapping windows
    from the input sequence, where each window has size [window_size].
    Each element in the output sequence is a list containing [window_size] consecutive
    elements from the input sequence.

    Example:
    Let's say we have a sequence [1; 2; 3; 4; 5] and window_size = 3
    sliding_window 3 (seq [1; 2; 3; 4; 5]) will produce:
    seq[[1; 2; 3]; [2; 3; 4]; [3; 4; 5]] *)
let sliding_window (window_size : int) (sequence : 'a Seq.t) : 'a list Seq.t =
  let indices = take window_size (ints 0) in
  let shifted_sequences = map (fun offset -> drop offset sequence) indices in
  let sequence_list = List.of_seq shifted_sequences in
  nzip sequence_list
;;

let add_prefix_padding sequence window_size min_window_size =
  let padding_length = window_size - min_window_size in
  Seq.append (Seq.take padding_length (Seq.repeat None)) sequence
;;
