open Containers
open Angstrom
open Aoc

type instr =
  | Swap of int * int
  | Replace of char * char
  | Rotate of int
  | Pivot of char
  | Reverse of int * int
  | Move of int * int
[@@deriving show]

let decoder =
  let posp = string "position " *> U.number in
  let letterp = string "letter " *> U.word >>| fun s -> s.[0] in
  let swapp =
    string "swap " *> posp <* string " with " >>= fun a ->
    posp >>| fun b -> Swap (a, b)
  in
  let replp =
    string "swap " *> letterp <* string " with " >>= fun a ->
    letterp >>| fun b -> Replace (a, b)
  in
  let rotp =
    string "rotate " *> U.word <* U.whitespace >>= fun dir ->
    U.number <* U.until_eol >>| fun x ->
    match dir with
    | "left" -> Rotate (-x)
    | "right" -> Rotate x
    | _ -> failwith "syntax error"
  in
  let pivotp =
    string "rotate based on position of letter " *> U.word >>| fun s -> Pivot s.[0]
  in
  let revp =
    lift2
      (fun a b -> Reverse (a, b))
      (string "reverse positions " *> U.number <* string " through ")
      U.number
  in
  let movep =
    string "move " *> posp <* string " to " >>= fun a ->
    posp >>| fun b -> Move (a, b)
  in
  choice [ swapp; replp; rotp; pivotp; revp; movep ]

let parse s =
  match parse_string ~consume:All decoder s with
  | Ok result -> result
  | Error msg -> failwith msg

let pivot c ca =
  let acc = Array.copy ca in
  let idx =
    match Array.find_idx (fun x -> Char.equal x c) acc with
    | Some (n, _) -> n
    | None -> failwith "can't find idx"
  in
  let idx' = if idx < 4 then idx + 1 else idx + 2 in
  rotate_right ~n:idx' acc;
  acc

let execute instructions s =
  let aux acc = function
    | Swap (a, b) ->
        let temp = acc.(a) in
        acc.(a) <- acc.(b);
        acc.(b) <- temp;
        acc
    | Replace (a, b) ->
        Array.map
          (function x when Char.equal x a -> b | x when Char.equal x b -> a | x -> x)
          acc
    | Rotate n ->
        if n < 0 then rotate_left ~n:(abs n) acc else rotate_right ~n acc;
        acc
    | Pivot c -> pivot c acc
    | Reverse (a, b) ->
        let rev_copy = Array.sub acc a (b - a + 1) |> Array.rev in
        for i = 0 to Array.length rev_copy - 1 do
          acc.(a + i) <- rev_copy.(i)
        done;
        acc
    | Move (a, b) ->
        let c = acc.(a) in
        let lst = Array.to_list acc in
        List.(remove_at_idx a lst |> insert_at_idx b c) |> Array.of_list
  in
  List.fold_left
    (fun x y ->
      Printf.printf "%s - %s\n" (String.of_array x) (show_instr y);
      aux x y)
    (String.to_array s) instructions

let execute_rev instructions s =
  let aux acc = function
    | Swap (b, a) ->
        let temp = acc.(a) in
        acc.(a) <- acc.(b);
        acc.(b) <- temp;
        acc
    | Replace (b, a) ->
        Array.map
          (function x when Char.equal x a -> b | x when Char.equal x b -> a | x -> x)
          acc
    | Rotate n ->
        if n < 0 then rotate_right ~n:(abs n) acc else rotate_left ~n acc;
        acc
    | Pivot c ->
        (* over here we just try and guess at the expected pivot by iteration *)
        let target = Array.copy acc in
        let rec aux curr =
          if String.equal (String.of_array target) (String.of_array (pivot c curr)) then
            curr
          else (
            rotate_left curr;
            aux curr)
        in
        aux acc
    | Reverse (a, b) ->
        let rev_copy = Array.sub acc a (b - a + 1) |> Array.rev in
        for i = 0 to Array.length rev_copy - 1 do
          acc.(a + i) <- rev_copy.(i)
        done;
        acc
    | Move (b, a) ->
        let c = acc.(a) in
        let lst = Array.to_list acc in
        List.(remove_at_idx a lst |> insert_at_idx b c) |> Array.of_list
  in
  List.fold_left
    (fun x y ->
      Printf.printf "%s - %s\n" (String.of_array x) (show_instr y);
      aux x y)
    (String.to_array s) (List.rev instructions)

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  let instructions = List.map parse data in
  let answer = execute instructions "abcdefgh" |> String.of_array in
  Printf.printf "part1 = %s\n" answer;
  let answer' = execute_rev instructions "fbgdceah" |> String.of_array in
  Printf.printf "part2 = %s\n" answer';
  let combinations = permutations (String.to_list "abcdefgh") in
  let result =
    List.find
      (fun s ->
        String.equal
          (execute instructions (String.of_list s) |> String.of_array)
          "fbgdceah")
      combinations
  in
  Printf.printf "brute force part2 = %s\n" (result |> String.of_list)
