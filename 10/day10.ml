open Containers
open Angstrom
open Aoc

type dest = Bot of int | Output of int [@@deriving show]

type instr =
  (* bot, value *)
  | Initial of int * int
  (* src bot, low dest, high dest *)
  | Give of int * dest * dest
[@@deriving show]

let decoder =
  let valuep =
    lift2
      (fun v b -> Initial (b, v))
      (string "value " *> U.number)
      (string " goes to bot " *> U.number)
  in
  let destp =
    U.whitespace *> peek_char_fail >>= function
    | 'b' -> lift (fun v -> Bot v) (U.word *> char ' ' *> U.number)
    | 'o' -> lift (fun v -> Output v) (U.word *> char ' ' *> U.number)
    | c -> failwith ("unknown char: " ^ String.of_char c)
  in
  let givep =
    lift3
      (fun b l h -> Give (b, l, h))
      (string "bot " *> U.number <* string " gives low to ")
      (destp <* string " and high to ")
      destp
  in
  peek_char_fail >>= function 'v' -> valuep | _ -> givep

let parse s =
  match parse_string ~consume:All decoder s with
  | Ok result -> result
  | Error msg -> failwith msg

(* type bot_state = { high : int; low : int } [@@deriving show] *)

let update_int_map_list k v m =
  (* Printf.printf "k=%d v=%d\n" k v; *)
  CCIntMap.update k (function None -> Some [ v ] | Some lst -> Some (v :: lst)) m

let make_initial_state lines =
  let bs' =
    List.fold_left
      (fun acc e ->
        match e with Initial (b, v) -> update_int_map_list b v acc | _ -> acc)
      CCIntMap.empty lines
  in
  let binstr' =
    List.fold_left
      (fun acc e ->
        match e with Give (b, ld, hd) -> CCIntMap.add b (ld, hd) acc | _ -> acc)
      CCIntMap.empty lines
  in
  (bs', binstr')

let find_bot_with_two_chips bs =
  CCIntMap.to_iter bs |> Iter.find_pred (fun (_, lst) -> List.length lst = 2)

let process instructions init_bs init_os =
  let rec aux bs os =
    match find_bot_with_two_chips bs with
    | None -> (bs, os)
    | Some (bidx, bchips) ->
        (* Printf.printf "%d - %s\n" bidx
           (bchips |> List.map string_of_int |> String.concat ";"); *)
        (* first remove bidx's chips *)
        let bs = CCIntMap.remove bidx bs in
        let a, b = pair_of_list bchips in
        let low_val, high_val = (min a b, max a b) in
        if low_val = 17 && high_val = 61 then Printf.printf "part 1 = %d\n" bidx;
        let ld, hd = CCIntMap.find_exn bidx instructions in
        let bs', os' =
          match (ld : dest) with
          | Bot b -> (update_int_map_list b low_val bs, os)
          | Output o -> (bs, update_int_map_list o low_val os)
        in
        let bs'', os'' =
          match (hd : dest) with
          | Bot b -> (update_int_map_list b high_val bs', os')
          | Output o -> (bs', update_int_map_list o high_val os')
        in
        aux bs'' os''
  in
  aux init_bs init_os

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
  let data' = List.map parse data in
  let bs, binstr = make_initial_state data' in
  CCIntMap.iter
    (fun k v ->
      Printf.printf "%d - %s\n" k (v |> List.map string_of_int |> String.concat ";"))
    bs;
  CCIntMap.iter
    (fun k (ld, hd) -> Printf.printf "%d - %s %s\n" k (show_dest ld) (show_dest hd))
    binstr;
  let os = CCIntMap.empty in
  let _, os' = process binstr bs os in
  CCIntMap.iter
    (fun k lst ->
      Printf.printf "Output %d - %s\n" k
        (lst |> List.map string_of_int |> String.concat ";"))
    os';
  let part2 =
    List.fold_left (fun acc x -> acc * List.hd (CCIntMap.find_exn x os')) 1 [ 0; 1; 2 ]
  in
  Printf.printf "part 2 = %d\n" part2
