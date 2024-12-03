open Printf

let print_table name data =
  let x, y = Seq.split data in
  let format_nums seq = seq |> Seq.map (sprintf "%8.2f") |> List.of_seq in
  let x_strs = format_nums x in
  let y_strs = format_nums y in
  let col_width = 8 in
  let header_width = max 8 (String.length "X") in
  let border =
    String.make (header_width + 2 + (List.length x_strs * (col_width + 3))) '-'
  in
  let x_row = sprintf "| %-*s |%s |" header_width "X" (String.concat " |" x_strs) in
  let y_row = sprintf "| %-*s |%s |" header_width "Y" (String.concat " |" y_strs) in
  sprintf "\n%s\n%s\n%s\n%s\n%s\n%s\n" name border x_row border y_row border
;;
