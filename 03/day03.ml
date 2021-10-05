open Containers
open Angstrom
open Aoc

let number = take_while1 P.is_digit >>| int_of_string

let triple =
  U.number <* U.whitespace >>= fun a ->
  U.number <* U.whitespace >>= fun b ->
  U.number >>= fun c -> return (a, b, c)

let parse_triple s =
  match parse_string ~consume:All triple s with
  | Ok (a, b, c) -> (a, b, c)
  | Error msg -> failwith msg

(* triangle test *)
let is_triangle (a, b, c) =
  let numbers = List.sort Int.compare [ a; b; c ] in
  match numbers with [ x; y; z ] when x + y > z -> true | _ -> false

let () =
  let data = read_lines "input.txt" |> List.map String.trim in
  let triples = List.map parse_triple data in
  let triangles = List.filter is_triangle triples in
  Printf.printf "part1 = %d\n" (List.length triangles);
  (* part 2 *)
  let c1, c2, c3 =
    List.fold_left
      (fun (c1, c2, c3) (x, y, z) -> (x :: c1, y :: c2, z :: c3))
      ([], [], []) triples
  in
  let triangles' =
    List.flatten [ c1; c2; c3 ]
    |> List.chunks 3
    |> List.map (function [ x; y; z ] -> (x, y, z) | _ -> failwith "bad")
    |> List.filter is_triangle
  in
  Printf.printf "part2 = %d\n" (List.length triangles')
(* alternatively using transpose *)
(* let a = List.map (fun (a, b, c) -> [ a; b; c ]) triples |> Aoc.transpose in *)
