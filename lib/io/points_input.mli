(** Stream of points read from standard input.
    Each point is represented as a float * float option.
    Returns None on invalid input or end of file. *)
val input_points_stream : (float * float) Seq.t
