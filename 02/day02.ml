open Containers
open Aoc

(* part 1 *)
let clamp_coord (x, y) =
  let xx = max (min x 1) (-1) in
  let yy = max (min y 1) (-1) in
  (xx, yy)

let coord_to_digit = function
  | -1, 1 -> '1'
  | -1, 0 -> '4'
  | -1, -1 -> '7'
  | 0, 1 -> '2'
  | 0, 0 -> '5'
  | 0, -1 -> '8'
  | 1, 1 -> '3'
  | 1, 0 -> '6'
  | 1, -1 -> '9'
  | _, _ -> failwith "errored coord"

let process =
  let process_line acc = List.fold_left (fun acc x -> walk acc x |> clamp_coord) acc in
  List.fold_left_map
    (fun acc lst ->
      let acc' = process_line acc lst in
      (acc', coord_to_digit acc'))
    (0, 0)

(* part 2*)
let clamp_coord2 (old_x, old_y) (new_x, new_y) =
  if Int.abs new_x + Int.abs new_y > 2 then (old_x, old_y) else (new_x, new_y)

let coord_to_digit2 = function
  | 0, 2 -> '1'
  | -1, 1 -> '2'
  | 0, 1 -> '3'
  | 1, 1 -> '4'
  | -2, 0 -> '5'
  | -1, 0 -> '6'
  | 0, 0 -> '7'
  | 1, 0 -> '8'
  | 2, 0 -> '9'
  | -1, -1 -> 'A'
  | 0, -1 -> 'B'
  | 1, -1 -> 'C'
  | 0, -2 -> 'D'
  | a, b -> failwith (Printf.sprintf "errored coord %d %d" a b)

let process2 =
  let process_line acc =
    List.fold_left (fun acc x -> walk acc x |> clamp_coord2 acc) acc
  in
  List.fold_left_map
    (fun acc lst ->
      let acc' = process_line acc lst in
      (acc', coord_to_digit2 acc'))
    (-2, 0)

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
  let data' =
    List.map
      (fun s ->
        String.to_list s
        |> List.map (fun x ->
               match x with
               | 'U' -> North
               | 'D' -> South
               | 'L' -> West
               | 'R' -> East
               | _ -> failwith "parse error"))
      data
  in
  let _, c = process data' in
  print_endline (String.of_list c);
  let _, c2 = process2 data' in
  print_endline (String.of_list c2)
