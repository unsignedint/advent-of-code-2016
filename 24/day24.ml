open Containers
open Printf
open Aoc
module BoardMap = Map.Make (Aoc.Coord)
module IntMap = Map.Make (Int)
module DistMap = Map.Make (Aoc.Coord)
module CoordSet = Set.Make (Aoc.Coord)

type coord = int * int [@@deriving show, eq, ord]

let make_board lines =
  let line_func board y line =
    List.foldi (fun acc x c -> BoardMap.add (x, y) c acc) board (String.to_list line)
  in
  List.foldi (fun acc y line -> line_func acc y line) BoardMap.empty lines

let find_sites board =
  BoardMap.fold
    (fun (k : coord) v acc ->
      match v with '0' .. '9' -> IntMap.add (Char.to_int v - 0x30) k acc | _ -> acc)
    board IntMap.empty

let print_grid board width height =
  (* unused diagnostic *)
  let xx = List.Infix.(0 --^ width) in
  let yy = List.Infix.(0 --^ height) in
  let lines =
    List.map
      (fun y ->
        List.map (fun x -> BoardMap.get_or (x, y) ~default:'*' board) xx
        |> String.of_list |> sprintf "%02d %s" y)
      yy
  in
  List.iter print_endline lines

type entry = { pos : coord; prev_site : int; num_steps : int } [@@deriving show]

let valid_moves board (x, y) =
  List.map (walk (x, y)) [ North; South; East; West ]
  |> List.filter (fun (x', y') -> x' >= 0 && x' < 179 && y' >= 0 && y' < 39)
  |> List.filter (fun coord ->
         match BoardMap.get coord board with Some '#' -> false | _ -> true)

let is_site board coord =
  match BoardMap.get coord board with
  | Some x -> ( match x with '0' .. '9' -> Some (Char.to_int x - 0x30) | _ -> None)
  | None -> None

let full_bfs board dist_table site start_pos =
  let rec aux seen dist_table q =
    if CCSimple_queue.is_empty q then dist_table
    else
      let { pos; prev_site; num_steps }, q = CCSimple_queue.pop_exn q in
      if CoordSet.mem pos seen then aux seen dist_table q
      else
        let seen' = CoordSet.add pos seen in
        let dist_table', site', steps' =
          match is_site board pos with
          | Some x when x <> prev_site ->
              ( DistMap.update (prev_site, x)
                  (function Some v -> Some (min v num_steps) | None -> Some num_steps)
                  dist_table,
                x,
                0 )
          | _ -> (dist_table, prev_site, num_steps)
        in
        let q' =
          valid_moves board pos
          |> List.map (fun pos -> { pos; prev_site = site'; num_steps = steps' + 1 })
          |> List.fold_left (fun acc candidate -> CCSimple_queue.push candidate acc) q
        in
        aux seen' dist_table' q'
  in
  aux CoordSet.empty dist_table
    CCSimple_queue.(empty |> push { pos = start_pos; prev_site = site; num_steps = 0 })

let make_total dist_map combo =
  let rec aux acc curr = function
    | [] -> acc
    | next :: xs ->
        let cost =
          match DistMap.get (curr, next) dist_map with
          | Some x -> x
          | _ -> failwith "what?"
        in
        aux (acc + cost) next xs
  in
  aux 0 (List.hd combo) (List.tl combo)

let () =
  let board = Aoc.read_lines "input.txt" |> List.map String.trim |> make_board in
  print_grid board 179 39;
  let sites = find_sites board in
  IntMap.iter (fun k v -> printf "%d - %s\n" k (show_coord v)) sites;
  print_endline " -- COMPUTING --\n";

  (* let result = full_bfs board DistMap.empty 0 (175, 1) in
     DistMap.iter (fun (x, y) v -> printf "(%d,%d) - %d\n" x y v) result *)
  let result = IntMap.fold (fun k v acc -> full_bfs board acc k v) sites DistMap.empty in
  DistMap.iter (fun (x, y) v -> printf "(%d,%d) - %d\n" x y v) result;
  let combinations =
    List.map (fun x -> (0 :: x) @ [ 0 ]) (permutations (List.range 1 7))
  in
  let with_totals = List.map (fun lst -> (make_total result lst, lst)) combinations in
  let sorted_combos = List.sort (fun x y -> compare (fst x) (fst y)) with_totals in
  let tot, _ = List.hd sorted_combos in
  Format.printf "%d\n" tot
