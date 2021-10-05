open Containers
open Angstrom

(* parsing *)
module P = struct
  let is_whitespace = function '\x20' | '\x0a' | '\x0d' | '\x09' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false
end

let whitespace = take_while P.is_whitespace

let number = take_while1 P.is_digit >>| int_of_string

let triple =
  number <* whitespace >>= fun a ->
  number <* whitespace >>= fun b ->
  number >>= fun c -> return (a, b, c)

let parse_triple s =
  match parse_string ~consume:All triple s with
  | Ok (a, b, c) -> (a, b, c)
  | Error msg -> failwith msg

(* triangle test *)
let is_triangle (a, b, c) =
  let numbers = List.sort Int.compare [ a; b; c ] in
  match numbers with [ x; y; z ] when x + y > z -> true | _ -> false

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
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
