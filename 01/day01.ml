open Base
open Stdio

type direction = North | East | South | West

type state = {
  direction: direction;
  x: int;
  y: int;
}

type turn = Right | Left
type move = { turn: turn; distance: int }

let update_direcion (old_direction: direction) (new_turn: turn) =
  match (new_turn, old_direction) with
  | (Left, North) -> West
  | (Left, West) -> South
  | (Left, South) -> East
  | (Left, East) -> North
  | (Right, North) -> East
  | (Right, East) -> South
  | (Right, South) -> West
  | (Right, West) -> North
;;

let update_state (old_state: state) (new_move: move) =
  let {x; y; _} = old_state in
  let direction = update_direcion old_state.direction new_move.turn in
  match direction with
  | North -> {direction; x; y=y+new_move.distance}
  | West -> {direction; x=x-new_move.distance;y}
  | South -> {direction; x;y=y-new_move.distance}
  | East -> {direction; x=x+new_move.distance;y}

let string_to_move s =
  let distance = Stdlib.String.sub s 1 (String.length s - 1) |> Int.of_string in
  match String.get s 0 with
  | 'R' ->  {turn=Right; distance}
  | 'L' ->  {turn=Left; distance}
  | _ -> failwith "error!!"


let () =
  let fragment = (Aoc.read_first_line "input.txt") |> String.split ~on:',' |> List.map ~f:String.strip in
  print_endline (String.concat ~sep:";" fragment) ;
  let result = List.fold 
    ~init:{direction=North; x=0; y=0}
    ~f:(fun acc x -> update_state acc (string_to_move x))
    fragment in
  printf "x=%d y=%d\n" result.x result.y;
  printf "blocks = %d" (Int.abs (result.x + result.y))

