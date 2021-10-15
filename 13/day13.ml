open Containers
open Format
open Aoc

type cell = Open | Wall

let char_of_cell = function Open -> '.' | Wall -> '#'

let get_cell secret (x, y) =
  let r = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) in
  let popcnt = Int.popcount (r + secret) in
  if popcnt land 1 = 1 then Wall else Open

let make_cell_func secret = get_cell secret

let print_grid cell_func width height =
  (* unused diagnostic *)
  let xx = List.Infix.(0 --^ width) in
  let yy = List.Infix.(0 --^ height) in
  let lines =
    List.map
      (fun y ->
        List.map (fun x -> cell_func (x, y)) xx
        |> List.map char_of_cell |> String.of_list |> sprintf "%02d %s" y)
      yy
  in
  List.iter print_endline lines

let neighbours (x, y) = List.map (walk (x, y)) [ North; East; South; West ]

type coord = int * int [@@deriving show, eq, ord]

type entry = { pos : coord; steps : int }

module CoordBFS = MakeBFS (struct
  type t = coord

  let compare = compare_coord
end)

let flood_fill cell_func start target =
  CoordBFS.bfs ~initial_state:{ pos = start; steps = 0 }
    ~f_end_condition:(fun { pos; _ } -> compare_coord pos target = 0)
    ~f_seen_kv:(fun { pos; steps } -> (pos, steps))
    ~f_make_candidates:(fun { pos; steps } ->
      neighbours pos
      |> List.filter (fun (x, y) -> x >= 0 && y >= 0)
      |> List.filter (fun pos -> match cell_func pos with Wall -> false | Open -> true)
      |> List.map (fun pos -> { pos; steps = steps + 1 }))

let () =
  (* print_grid (make_cell_func 10) 10 7; *)
  let result, seen = flood_fill (make_cell_func 1362) (1, 1) (31, 39) in
  printf "!part1 = %d\n" result.steps;
  printf "!part2 = %d\n" CoordBFS.SeenMap.(filter (fun _ v -> v <= 50) seen |> cardinal)
