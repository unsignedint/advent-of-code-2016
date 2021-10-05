open Containers
open Angstrom

let code =
  Aoc.(
    sep_by (char '-') U.word <* char '-' >>= fun a ->
    U.number >>= fun b ->
    char '[' *> U.word <* char ']' >>= fun c -> return (String.concat "" a, b, c))

type entry = { name : string; code : int; checksum : string }

let parse_line l =
  match parse_string ~consume:All code l with
  | Ok (name, code, checksum) -> { name; code; checksum }
  | Error msg -> failwith msg

module CharHashtbl = CCHashtbl.Make (struct
  type t = char

  let equal x y = Char.code x = Char.code y

  let hash = Char.code
end)

let check_entry entry =
  let h = CharHashtbl.create 10 in
  String.iter (fun c -> CharHashtbl.incr ~by:1 h c) entry.name;
  (* CharHashtbl.iter (fun k v -> Printf.printf "%c - %d\n" k v) h; *)
  let top5 =
    CharHashtbl.to_list h
    |> List.sort (fun (k1, v1) (k2, v2) ->
           match Int.compare v2 v1 with 0 -> Char.compare k1 k2 | c -> c)
    |> List.take 5
  in
  (* List.iter (fun (k, v) -> Printf.printf "%c %d\n" k v) top5; *)
  let phrase = List.map fst top5 |> String.of_list in
  (* Printf.printf "phrase = %s, match = %B\n" phrase (String.equal phrase entry.checksum) ; *)
  if String.equal phrase entry.checksum then Some entry else None

(* part 2 *)
let rotate_string n s =
  (* lower case only! *)
  String.to_array s
  |> Array.map (fun c -> Char.to_int c - Char.to_int 'a')
  |> Array.map (fun c -> (c + n) mod 26)
  |> Array.map (fun c -> c + Char.to_int 'a')
  |> Array.map Char.of_int_exn |> String.of_array

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
  let entries = List.map parse_line data |> List.map check_entry |> List.keep_some in
  let code = List.fold_left (fun acc e -> acc + e.code) 0 entries in
  Printf.printf "part1 = %d\n" code;
  let decrypted = List.map (fun e -> (rotate_string e.code e.name, e.code)) entries in
  List.iter (fun (n, c) -> Printf.printf "%s %d\n" n c) decrypted
