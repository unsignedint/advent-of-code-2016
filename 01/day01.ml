open Printf

type direction = North | East | South | West

let int_of_direction = function North -> 0 | East -> 1 | South -> 2 | West -> 3

let direction_of_int = function
  | 0 -> North
  | 1 -> East
  | 2 -> South
  | 3 -> West
  | _ -> failwith "Invalid int for direction"

module VisitedSet = Set.Make (struct
  type t = int * int

  let compare = compare
end)

type state = { direction : direction; x : int; y : int }

type turn = Right | Left

type move = { turn : turn; distance : int }

let update_direcion (old_direction : direction) (new_turn : turn) =
  let a =
    match new_turn with Left -> 3 (* because mod can be negative we move 270' *) | Right -> 1
  in
  direction_of_int ((int_of_direction old_direction + a) mod 4)
(*
   match (new_turn, old_direction) with
   | (Left, North) -> West
   | (Left, West) -> South
   | (Left, South) -> East
   | (Left, East) -> North
   | (Right, North) -> East
   | (Right, East) -> South
   | (Right, South) -> West
   | (Right, West) -> North *)

let update_state (old_state : state) (new_move : move) =
  let { x; y; _ } = old_state in
  let direction = update_direcion old_state.direction new_move.turn in
  match direction with
  | North -> { direction; x; y = y + new_move.distance }
  | West -> { direction; x = x - new_move.distance; y }
  | South -> { direction; x; y = y - new_move.distance }
  | East -> { direction; x = x + new_move.distance; y }

type update_result = NotFound of state * VisitedSet.t | Found of int * int

let update_state2 ((old_state : state), (visited : VisitedSet.t)) (new_move : move) =
  let new_state = update_state old_state new_move in
  let old_x, old_y, new_x, new_y = (old_state.x, old_state.y, new_state.x, new_state.y) in
  if old_x = new_x then
    let indices = CCList.range' old_y new_y in
    let x_pos = old_x in
    let rec aux v = function
      | [] -> NotFound (new_state, v)
      | y_pos :: ys ->
          if VisitedSet.mem (x_pos, y_pos) v then Found (x_pos, y_pos)
          else aux (VisitedSet.add (x_pos, y_pos) v) ys
    in
    aux visited indices
  else if old_y = new_y then
    let indices = CCList.range' old_x new_x in
    let y_pos = old_y in
    let rec aux v = function
      | [] -> NotFound (new_state, v)
      | x_pos :: xs ->
          if VisitedSet.mem (x_pos, y_pos) v then Found (x_pos, y_pos)
          else aux (VisitedSet.add (x_pos, y_pos) v) xs
    in
    aux visited indices
  else failwith "Invalid Update"

let string_to_move s =
  let distance = String.sub s 1 (String.length s - 1) |> int_of_string in
  match String.get s 0 with
  | 'R' -> { turn = Right; distance }
  | 'L' -> { turn = Left; distance }
  | _ -> failwith "error!!"

let () =
  let fragment =
    Aoc.read_first_line "input.txt" |> String.split_on_char ',' |> List.map String.trim
  in
  print_endline (String.concat ";" fragment);

  (* part 1 *)
  let result =
    List.fold_left
      (fun acc x -> update_state acc (string_to_move x))
      { direction = North; x = 0; y = 0 }
      fragment
  in
  printf "x=%d y=%d\n" result.x result.y;
  printf "blocks = %d\n" (Int.abs (result.x + result.y));

  (* part 2 *)
  let rec aux st v = function
    | [] -> None
    | m_str :: ms -> (
        let m = string_to_move m_str in
        match update_state2 (st, v) m with
        | NotFound (st', v') -> aux st' v' ms
        | Found (x, y) -> Some (x, y))
  in
  match aux { direction = North; x = 0; y = 0 } VisitedSet.empty fragment with
  | None -> ()
  | Some (x, y) -> printf "found %d %d - dist %d" x y (Int.abs (x + y))
