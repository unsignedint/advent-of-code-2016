open Containers
open Aoc
open Format

(*
NOTE! the coordinates are a bit weird for this one, because
we assume top-left of the grid is (0,3) and the goal is bottom-right (3,0)
*)
let secret = "mmsxrhfx"

type door = Open | Closed

let dir_to_char = function North -> "U" | South -> "D" | West -> "L" | East -> "R"

let valid_moves (x, y) path =
  Digest.(string (secret ^ path) |> to_hex)
  |> (fun s -> String.sub s 0 4 |> String.to_list)
  |> List.map (function 'a' .. 'f' -> Open | _ -> Closed)
  |> List.combine [ North; South; West; East ]
  |> List.filter_map (function
       | North, Open when y < 3 -> Some North
       | South, Open when y > 0 -> Some South
       | West, Open when x > 0 -> Some West
       | East, Open when x < 3 -> Some East
       | _, _ -> None)

type coord = int * int [@@deriving show, eq, ord]

type entry = { pos : coord; path : string }

let part1 =
  let rec aux q =
    let { pos; path }, q = CCSimple_queue.pop_exn q in
    (* check end condition *)
    if compare_coord pos (3, 0) = 0 then path
    else
      let q' =
        valid_moves pos path
        |> List.map (fun m ->
               let pos = Aoc.walk pos m in
               let path = path ^ dir_to_char m in
               { pos; path })
        |> List.fold_left (fun acc candidate -> CCSimple_queue.push candidate acc) q
      in
      aux q'
  in
  aux CCSimple_queue.(empty |> push { pos = (0, 3); path = "" })

let part2 =
  let rec aux paths q =
    if CCDeque.is_empty q then paths
    else
      let { pos; path } = CCDeque.take_front q in
      printf "%d\n" (List.length paths);
      (* check end condition *)
      if compare_coord pos (3, 0) = 0 then aux (path :: paths) q
      else (
        valid_moves pos path
        |> List.map (fun m ->
               let pos = Aoc.walk pos m in
               let path = path ^ dir_to_char m in
               { pos; path })
        |> List.iter (fun candidate -> CCDeque.push_back q candidate);
        aux paths q)
  in
  let q = CCDeque.create () in
  CCDeque.push_back q { pos = (0, 3); path = "" };
  aux [] q

let () =
  let solve = part1 in
  printf "part1 = %s\n" solve;
  let solve = part2 in
  printf "part2 = %d\n" (String.length (List.hd solve))
