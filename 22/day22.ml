open Containers
open Angstrom
open Aoc
open Printf

type record = { x : int; y : int; size : int; used : int; avail : int } [@@deriving show]

let decoder =
  let nodep =
    lift2
      (fun x y -> (x, y))
      (string "/dev/grid/node-x" *> U.number)
      (string "-y" *> U.number)
  in
  let sizep = U.whitespace *> U.number <* char 'T' in
  (fun (x, y) -> function
    | [ size; used; avail ] -> { x; y; size; used; avail }
    | _ -> failwith "err")
  <$> nodep <*> count 3 sizep <* U.until_eol

let parse s =
  match parse_string ~consume:All decoder s with
  | Ok result -> result
  | Error msg -> failwith msg

let process data =
  let used = List.map (fun { used; _ } -> used) data |> List.sort Int.compare in
  let avail =
    List.map (fun { avail; _ } -> avail) data |> List.sort Int.compare |> List.rev
  in
  (used, avail)

let viable_pairs used avail =
  let find_num_candidates u = List.take_while (fun a -> u <= a) avail |> List.length in
  List.fold_left (fun acc x -> acc + find_num_candidates x) 0 (List.tl used)

module BoardMap = Map.Make (Aoc.Coord)

type coord = int * int [@@deriving show, eq, ord]

type entry = { pos : coord; path : coord list; moves : int } [@@deriving show]

module SearchHeap = CCHeap.Make (struct
  type t = int * entry

  let leq (w, _) (v, _) = w <= v
end)

module SeenMap = Map.Make (Aoc.Coord)

let populate_board data =
  let aux = function 0 -> '_' | x when x < 88 -> '.' | _ -> '#' in
  List.fold_left
    (fun acc { x; y; used; _ } -> BoardMap.add (x, y) (aux used) acc)
    BoardMap.empty data

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

let valid_moves board (x, y) =
  List.map (walk (x, y)) [ North; South; East; West ]
  |> List.filter (fun (x', y') -> x' >= 0 && x' < 33 && y' >= 0 && y' < 31)
  |> List.filter (fun coord ->
         match BoardMap.get coord board with Some '.' -> true | _ -> false)

let error (goal_x, goal_y) (x, y) = abs (goal_x - x) + abs (goal_y - y)

let search board goal_coord start_coord =
  let rec aux seen q =
    let q, (w, { pos; path; moves }) = SearchHeap.take_exn q in
    printf "taken: %s\n" (show_coord pos);
    (* end of successful path *)
    if compare_coord pos goal_coord = 0 then path
    else if SeenMap.mem pos seen then aux seen q
    else
      let seen' = SeenMap.add pos w seen in
      let q' =
        valid_moves board pos
        |> List.fold_left
             (fun acc pos' ->
               SearchHeap.add acc
                 ( error goal_coord pos' + moves,
                   { pos = pos'; path = pos' :: path; moves = moves + 1 } ))
             q
      in
      aux seen' q'
  in
  let first_entry =
    (error goal_coord start_coord, { pos = start_coord; path = []; moves = 0 })
  in
  aux SeenMap.empty SearchHeap.(add empty first_entry)

let () =
  let data = Aoc.read_lines "input.txt" |> List.drop 2 in
  let rows = List.map parse data in
  (* List.iter (fun r -> printf "%s\n" (show_record r)) rows; *)
  let used, avail = process rows in
  (* List.iter (fun r -> printf "avail %d\n" r) avail;
     List.iter (fun r -> printf "used %d\n" r) used; *)
  printf "part1 = %d\n" (viable_pairs used avail);
  List.filter (fun { used; avail; _ } -> used <= avail) rows
  |> List.iter (fun r -> printf "%s\n" (show_record r));
  print_endline "---";
  let board = populate_board rows in
  print_grid board 33 31;
  let start_coord = (3, 28) in
  let goal_coord = (32, 0) in
  let path = search board goal_coord start_coord in
  List.iter (fun coord -> print_endline (show_coord coord)) path;
  let board_path =
    List.fold_left (fun acc coord -> BoardMap.add coord '.' acc) BoardMap.empty path
  in
  print_grid board_path 33 31;
  printf "length path = %d\n" (List.length path)

(*
    based on this initial A* search we found it took
    63 moves to get to this state =>

    #..............................G_

    now, need 31 more "rotations" at 5 moves each to get `G` to `#` =>
    63 + (31 * 5) = 218 moves
  *)
