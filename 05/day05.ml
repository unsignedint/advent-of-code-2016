open Containers

(* part 1 *)
let get_code s =
  let check_hash i =
    let hex_digest = Digest.(string (s ^ string_of_int i) |> to_hex) in
    if String.prefix ~pre:"00000" hex_digest then Some hex_digest.[5] else None
  in
  Iter.init (fun x -> x)
  |> Iter.map check_hash |> Iter.keep_some |> Iter.take 8 |> String.of_iter

(* part 2 *)
let get_code2 s =
  let check_hash i =
    let hex_digest = Digest.(string (s ^ string_of_int i) |> to_hex) in
    if String.prefix ~pre:"00000" hex_digest then
      Some (Char.to_int hex_digest.[5] - 0x30, hex_digest.[6])
    else None
  in
  let upd_code_array (acc : char array) (pos, (c : char)) =
    Printf.printf "%d %c\n" pos c;
    if pos < 8 && Char.equal acc.(pos) '!' then acc.(pos) <- c;
    if not (String.contains (String.of_array acc) '!') then (acc, `Stop)
    else (acc, `Continue)
  in
  Iter.init (fun x -> x)
  |> Iter.map check_hash |> Iter.keep_some
  |> Iter.fold_while upd_code_array (String.to_array "!!!!!!!!")
  |> String.of_array

let () =
  let data = Aoc.read_first_line "input.txt" |> String.trim in
  print_endline data;
  Printf.printf "part1 = %s\n" (get_code data);
  Printf.printf "part2 = %s\n" (get_code2 data)
