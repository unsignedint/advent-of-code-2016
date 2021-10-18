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

type coord = int * int [@@deriving show, eq, ord]

type entry = { pos : coord; path : string } [@@deriving show]

let valid_moves (x, y) path =
  Digest.(string (secret ^ path) |> to_hex)
  |> (fun s -> String.sub s 0 4 |> String.to_list)
  |> List.combine [ North; South; West; East ]
  |> List.filter_map (function
       | dir, 'b' .. 'f' ->
           let x, y = Aoc.walk (x, y) dir in
           let path = path ^ dir_to_char dir in
           Some { pos = (x, y); path }
       | _, _ -> None)
  |> List.filter (fun { pos = x, y; _ } -> x >= 0 && x < 4 && y >= 0 && y < 4)

let full_bfs =
  let rec aux paths q =
    if CCSimple_queue.is_empty q then paths
    else
      let { pos; path }, q = CCSimple_queue.pop_exn q in
      (* end of successful path *)
      if compare_coord pos (3, 0) = 0 then aux (path :: paths) q
      else
        let q' =
          valid_moves pos path
          |> List.fold_left (fun acc candidate -> CCSimple_queue.push candidate acc) q
        in
        aux paths q'
  in
  aux [] CCSimple_queue.(empty |> push { pos = (0, 3); path = "" })

let () =
  let paths = full_bfs in
  printf "part1 = %s\n" (List.rev paths |> List.hd);
  printf "part2 = %d\n" (List.hd paths |> String.length)
