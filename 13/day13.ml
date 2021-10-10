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

module CoordSet = CCSet.Make (struct
  type t = coord

  let compare = compare_coord
end)

let search cell_func start target =
  let rec aux seen q =
    let { pos; steps }, q = CCSimple_queue.pop_exn q in
    if compare_coord pos target = 0 then steps
    else
      (* add to seen set! *)
      let seen' = CoordSet.add pos seen in
      (* add new frontier to queue *)
      let q' =
        neighbours pos
        |> List.filter (fun pos ->
               match cell_func pos with Wall -> false | Open -> true)
        |> List.filter (fun pos -> not (CoordSet.mem pos seen))
        |> List.fold_left
             (fun acc pos -> CCSimple_queue.push { pos; steps = steps + 1 } acc)
             q
      in
      aux seen' q'
  in
  aux CoordSet.empty CCSimple_queue.(empty |> push { pos = start; steps = 0 })

module CoordMap = CCMap.Make (struct
  type t = coord

  let compare = compare_coord
end)

let flood cell_func start num_steps =
  let rec aux seen q =
    let { pos; steps }, q = CCSimple_queue.pop_exn q in
    if steps = num_steps then seen
    else
      (* add to seen set! *)
      let seen' = CoordMap.add pos steps seen in
      (* add new frontier to queue *)
      let q' =
        neighbours pos
        |> List.filter (fun (x, y) -> x >= 0 && y >= 0)
        |> List.filter (fun pos ->
               match cell_func pos with Wall -> false | Open -> true)
        |> List.filter (fun pos -> not (CoordMap.mem pos seen))
        |> List.fold_left
             (fun acc pos -> CCSimple_queue.push { pos; steps = steps + 1 } acc)
             q
      in
      aux seen' q'
  in
  aux CoordMap.empty CCSimple_queue.(empty |> push { pos = start; steps = 0 })

let () =
  (* print_grid (make_cell_func 10) 10 7; *)
  let result = search (make_cell_func 1362) (1, 1) (31, 39) in
  printf "part1 = %d\n" result;
  let raw = flood (make_cell_func 1362) (1, 1) 60 in
  let result = CoordMap.filter (fun _ v -> v <= 50) raw |> CoordMap.cardinal in
  printf "part2 = %d\n" result
(* CoordMap.iter (fun k v -> printf "%s - %d\n" (show_coord k) v) raw *)
