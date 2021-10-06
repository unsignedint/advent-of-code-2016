open Containers
open Aoc

let solve s =
  let h = CharHashtblU.from_string s in
  CharHashtblU.sorted_v_k h |> List.hd |> fst

let solve2 s =
  let h = CharHashtblU.from_string s in
  CharHashtblU.sorted_v_k h |> List.rev |> List.hd |> fst

let () =
  let columns =
    read_lines "input.txt" |> List.map String.trim |> List.map String.to_list |> transpose
    |> List.map String.of_list
  in
  print_endline (String.concat ";" columns);
  List.map solve columns |> String.of_list |> Printf.printf "part1 = %s\n";
  List.map solve2 columns |> String.of_list |> Printf.printf "part2 = %s\n"
