open Containers
open Format

let round s =
  let a = String.to_array s in
  let b =
    Array.map (function '1' -> '0' | '0' -> '1' | _ -> failwith "invalid input") a
    |> Array.rev
  in
  String.of_array a ^ "0" ^ String.of_array b

let checksum (s : string) =
  let rec aux s =
    let cs =
      List.chunks 2 s
      |> List.map (function [ a; b ] when Char.equal a b -> '1' | _ -> '0')
    in
    if List.length cs mod 2 = 1 then String.of_list cs else aux cs
  in
  aux (String.to_list s)

let fill_disk s count =
  let rec aux s =
    if String.length s >= count then String.sub s 0 count else aux (round s)
  in
  aux s

let () =
  printf "part1 = %s\n" (checksum (fill_disk "01111001100111011" 272));
  printf "part2 = %s\n" (checksum (fill_disk "01111001100111011" 35651584))
