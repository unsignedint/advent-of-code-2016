
let read_all_lines fname = CCIO.(with_in fname read_lines_l);;

let read_lines fname =
  read_all_lines fname
  |> List.map String.trim
  |> List.filter (fun s -> not (CCString.is_empty s))
;;

let read_first_line fname = List.hd (read_lines fname)
;;
