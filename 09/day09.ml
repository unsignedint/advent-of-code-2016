open Containers
open Angstrom
open Aoc

let string_repeat s n = Array.fold_left ( ^ ) "" (Array.make n s)

let decoder =
  let byp =
    char '(' *> U.number <* char 'x' >>= fun x ->
    U.number <* char ')' >>= fun n ->
    take x >>= fun s -> return (string_repeat s n)
  in
  many (choice [ U.word; byp ])

let parse s =
  match parse_string ~consume:All decoder s with
  | Ok result -> String.(concat "" result)
  | Error msg -> failwith msg

(* part 2

   for this we slightly modify the parser to return a tuple of (count, fragment)
   which is then recurisvely processed and flattended until there are no more markers
*)
let decoder2 =
  let byp =
    char '(' *> U.number <* char 'x' >>= fun x ->
    U.number <* char ')' >>= fun n ->
    take x >>= fun s -> return (n, s)
  in
  let wordp = U.word >>= fun s -> return (1, s) in
  many (choice [ wordp; byp ])

let parse2 s =
  let rec aux m s =
    match parse_string ~consume:All decoder2 s with
    | Ok result ->
        List.map (fun (n, s) -> (n * m, s)) result
        |> List.map (fun (n, s) -> if String.contains s '(' then aux n s else [ (n, s) ])
        |> List.flatten
    | Error msg -> failwith msg
  in
  aux 1 s

let () =
  let data = read_first_line "input.txt" |> String.trim in
  let part1 =
    match parse_string ~consume:All decoder data with
    | Ok result -> String.(concat "" result |> length)
    | Error msg -> failwith msg
  in
  Printf.printf "part1 = %d\n" part1;
  let result = parse2 data in
  let part2 = List.fold_left (fun acc (n, s) -> acc + (n * String.length s)) 0 result in
  Printf.printf "part2 = %d\n" part2
