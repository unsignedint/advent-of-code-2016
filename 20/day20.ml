open Containers
open Aoc
open Angstrom

let decoder = lift2 (fun a b -> (a, b)) (U.number <* char '-') U.number

let parse s =
  match parse_string ~consume:All decoder s with
  | Ok result -> result
  | Error msg -> failwith msg

(* scan _sorted_ list of intervals to find overlapping ranges *)
let merge ranges =
  let rec aux acc curr_start curr_end = function
    | (a, b) :: xs ->
        if curr_end + 1 < a then
          (* new range identified *)
          aux ((curr_start, curr_end) :: acc) a b xs
        else
          let new_end = max curr_end b in
          aux acc curr_start new_end xs
    | [] ->
        (* remember to add the final pending range! *)
        List.rev ((curr_start, curr_end) :: acc)
  in
  aux [] 0 0 ranges

let count_holes ranges =
  let rec aux acc prev_end = function
    | (a, b) :: xs ->
        let acc' = a - (prev_end + 1) in
        aux (acc' + acc) b xs
    | [] -> acc
  in
  aux 0 (snd (List.hd ranges)) (List.tl ranges)

let test i ranges = List.exists (fun (a, b) -> i >= a && i <= b) ranges

let smart_part1 ranges =
  let rec aux curr = function
    (* use a max function because it may be a smaller range that our current value *)
    | (a, b) :: xs -> if curr < a then curr else aux (max curr (b + 1)) xs
    | [] -> failwith "end"
  in
  aux (snd (List.hd ranges) + 1) (List.tl ranges)

let part1 ranges =
  Iter.init Aoc.identity |> Iter.filter (fun i -> not (test i ranges)) |> Iter.head_exn

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  let ranges = List.map parse data in
  let sorted_ranges = List.sort (fun a b -> compare (fst a) (fst b)) ranges in
  Format.printf "part1 = %d\n" (smart_part1 sorted_ranges);
  (* part 2 needed something smarter then brute force *)
  let new_ranges = merge sorted_ranges |> List.filter (fun (a, _) -> a <= 4294967295) in
  Format.printf "part2 = %d\n" (count_holes new_ranges)
