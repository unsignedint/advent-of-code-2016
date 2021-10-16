open Containers
open Format

let round s =
  let a = String.to_array s in
  let b =
    Array.rev a
    |> Array.map (function '1' -> '0' | '0' -> '1' | _ -> failwith "invalid input")
  in
  String.of_array a ^ "0" ^ String.of_array b

let checksum (s : string) =
  let rec aux s =
    let cs =
      String.to_list s |> List.chunks 2 |> List.map Aoc.pair_of_list
      |> List.map (function a, b when Char.equal a b -> '1' | _, _ -> '0')
      |> String.of_list
    in
    if String.length cs mod 2 = 1 then cs else aux cs
  in
  aux s

let fill_disk s count =
  let rec aux s =
    if String.length s >= count then String.sub s 0 count else aux (round s)
  in
  aux s

let () =
  printf "part1 = %s\n" (checksum (fill_disk "01111001100111011" 272));
  printf "part2 = %s\n" (checksum (fill_disk "01111001100111011" 35651584))
