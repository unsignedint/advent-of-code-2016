open Containers
(* open Angstrom
   open Aoc *)

type element = Chip of string | Generator of string [@@deriving show, eq, ord]

type move = Down of element list | Up of element list [@@deriving show]

type st = element list array * int [@@deriving show, eq, ord]

module State = struct
  type t = st

  let compare = compare_st
end

type q_entry = { state : st; num_steps : int }

type seen_entry = { num_pairs : int; num_chips : int; num_gens : int }
[@@deriving show, eq, ord]

type seen_struct = seen_entry array * int [@@deriving show, eq, ord]

module SeenState = struct
  type t = seen_struct

  let compare = compare_seen_struct
end

(* let initial =
   ( [|
       [ Chip "hydrogen"; Chip "lithium" ];
       [ Generator "hydrogen" ];
       [ Generator "lithium" ];
       [];
     |],
     0 ) *)
let initial =
  ( [|
      [ Chip "promethium"; Generator "promethium" ];
      [
        Generator "cobalt";
        Generator "curium";
        Generator "ruthenium";
        Generator "plutonium";
      ];
      [ Chip "cobalt"; Chip "curium"; Chip "ruthenium"; Chip "plutonium" ];
      [];
    |],
    0 )

module StringSet = Set.Make (String)
module StateSet = Set.Make (State)

let make_seen_entry lst =
  let chips, gens =
    List.partition_filter_map (function Chip x -> `Left x | Generator x -> `Right x) lst
  in
  let chips', gens' = (StringSet.of_list chips, StringSet.of_list gens) in
  let inter = StringSet.inter chips' gens' in
  let rem_chips = StringSet.diff chips' inter in
  let rem_gens = StringSet.diff gens' inter in
  {
    num_pairs = StringSet.cardinal inter;
    num_chips = StringSet.cardinal rem_chips;
    num_gens = StringSet.cardinal rem_gens;
  }

let seen_struct_of_st st =
  let data, floor_idx = st in
  let data' = Array.map make_seen_entry data in
  (data', floor_idx)

module SeenStateSet = Set.Make (SeenState)

let print_state (data, floor_idx) =
  Array.iteri
    (fun i lst ->
      Printf.printf "%cF%d - %s\n"
        (if i = floor_idx then '*' else ' ')
        (i + 1)
        (String.concat " " (List.map show_element lst)))
    data;
  print_endline ""

let bad_floor lst =
  let chips, gens =
    List.partition_filter_map (function Chip x -> `Left x | Generator x -> `Right x) lst
  in
  let chips', gens' = (StringSet.of_list chips, StringSet.of_list gens) in
  let rem_chips = StringSet.diff chips' gens' in
  StringSet.cardinal rem_chips >= 1 && StringSet.cardinal gens' >= 1

let find_moves lst =
  if List.length lst = 0 then failwith "Invalid state, nothing to move"
  else
    let all_pairs = List.diagonal lst |> List.map Aoc.list_of_pair in
    let all_singles = List.map (fun el -> [ el ]) lst in
    let result =
      [
        List.map (fun el -> Up el) all_pairs
        @ List.map (fun el -> Down el) all_singles
        @ List.map (fun el -> Up el) all_singles
        @ List.map (fun el -> Down el) all_pairs;
      ]
    in
    List.flatten result

(* let chips, gens =
     List.partition_filter_map
       (function Chip x -> `Left x | Generator x -> `Right x)
       lst
   in
   let chips', gens' = (StringSet.of_list chips, StringSet.of_list gens) in
   let inter = StringSet.inter chips' gens' in
   (* let rem_chips = StringSet.diff chips' inter in
      let rem_gens = StringSet.diff gens' inter in *)
   (* iterate pairs first *)
   let acc =
     List.fold_left
       (fun acc el -> Up [ Chip el; Generator el ] :: acc)
       [] (StringSet.to_list inter)
   in
   (* then all chips individually *)
   List.fold_left
     (fun acc el -> Up [ Chip el ] :: Down [ Chip el ] :: acc)
     acc (StringSet.to_list chips') *)
(* in
   List.fold_left
     (fun acc el -> Up [ Generator el ] :: Down [ Generator el ] :: acc)
     acc' (StringSet.to_list rem_gens) *)

let make_candidate state floor_idx move =
  (* Printf.printf "MOVE %s\n" (show_move move); *)
  let floor = state.(floor_idx) in
  let new_state = Array.copy state in
  match move with
  | Up els ->
      if floor_idx = 3 then None
      else
        let up_floor = els @ state.(floor_idx + 1) in
        let curr_floor =
          List.fold_left
            (fun acc el -> List.filter (fun el' -> not (equal_element el el')) acc)
            floor els
        in
        new_state.(floor_idx) <- curr_floor;
        new_state.(floor_idx + 1) <- up_floor;
        Some (new_state, floor_idx + 1)
  | Down els ->
      if floor_idx = 0 then None
      else
        let down_floor = els @ state.(floor_idx - 1) in
        let curr_floor =
          List.fold_left
            (fun acc el -> List.filter (fun el' -> not (equal_element el el')) acc)
            floor els
        in
        new_state.(floor_idx) <- curr_floor;
        new_state.(floor_idx - 1) <- down_floor;
        Some (new_state, floor_idx - 1)

let find_candidates (state, floor_idx) =
  let floor = state.(floor_idx) in
  List.map (fun move -> make_candidate state floor_idx move) (find_moves floor)
  |> List.keep_some

let valid_state (state, _) = Array.filter bad_floor state |> Array.length = 0

(* let process initial_state =
   let rec aux acc st =
     let data, floor_idx = st in
     if floor_idx = 4 && List.length data.(floor_idx) = 4 then (
       print_endline "FOUND/?!";
       (acc, `Stop))
     else
       let next_states = candidates st |> List.filter valid_state in
       List.fold_while (fun acc ns -> (aux (acc + 1) ns, `Stop)) acc next_states
   in
   aux 0 initial_state *)

(* let process initial_state =
   let rec aux steps seen = function
     | st :: xs ->
         if StateSet.mem st seen then aux steps seen xs
         else (
           Printf.printf "step (%d)\n%s\nvalid - %B\n" steps (show_st st) (valid_state st);
           let seen' = StateSet.add st seen in
           let data, floor_idx = st in
           if floor_idx = 3 && List.length data.(floor_idx) = 4 then (
             print_endline "FOUND/?!";
             Some (steps, st))
           else
             let new_candidates = find_candidates st |> List.filter valid_state in
             aux (steps + 1) seen' (new_candidates @ xs))
     | [] -> None
   in
   aux 0 StateSet.empty [ initial_state ] *)

let process initial_state =
  let rec aux seen q =
    if CCSimple_queue.is_empty q then failwith "not found?"
    else
      let entry, q' = CCSimple_queue.pop_exn q in
      let st = entry.state in
      let num_steps = entry.num_steps in
      if SeenStateSet.mem (seen_struct_of_st st) seen then aux seen q'
      else (
        Printf.printf "step (%d) valid - %B\n" num_steps (valid_state st);
        (* print_state st; *)
        let seen' = SeenStateSet.add (seen_struct_of_st st) seen in
        let data, floor_idx = st in
        if floor_idx = 3 && List.length data.(floor_idx) = 10 then (
          Printf.printf "FOUND/?! => %d\n" num_steps;
          aux seen' q' (* (num_steps, st)) *))
        else
          let new_candidates = find_candidates st |> List.filter valid_state in
          let q'' =
            List.fold_left
              (fun acc c ->
                CCSimple_queue.push { state = c; num_steps = num_steps + 1 } acc)
              q' new_candidates
          in
          aux seen' q'')
  in

  aux SeenStateSet.empty
    CCSimple_queue.(empty |> push { state = initial_state; num_steps = 0 })

let () =
  (* let data = Aoc.read_lines "input.txt" |> List.map String.trim in
     print_endline (String.concat ";" data) *)
  (* let next_states = candidates initial in
     List.iter
       (fun s -> Printf.printf "STATE\n%s\nvalid - %B\n" (show_st s) (valid_state s))
       next_states *)
  (* let s = List.hd (process initial) in
     Printf.printf "STATE\n%s\nvalid - %B\n" (show_st s) (valid_state s) *)
  let steps, s = process initial in
  Printf.printf "FINAL %d\n%s\nvalid - %B\n" steps (show_st s) (valid_state s)
