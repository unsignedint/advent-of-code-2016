open Containers
open Format

let make_digest seed i = Digest.(string (seed ^ string_of_int i) |> to_hex)

let make_digest_part2 seed =
  Aoc.memoise (fun x ->
      Aoc.repeat_fold
        (fun acc _ -> Digest.(string acc |> to_hex))
        (make_digest seed x) 2016)

let find_triple s =
  let rec aux acc = function
    | [] -> None
    | x :: xs ->
        let c, count = acc in
        if Char.equal x c then if count = 2 then Some x else aux (x, count + 1) xs
        else aux (x, 1) xs
  in
  aux ('~', 0) (String.to_list s)

let test_penta_char c s =
  let rec aux count = function
    | [] -> false
    | x :: xs when Char.equal x c -> if count = 4 then true else aux (count + 1) xs
    | _ :: xs -> aux 0 xs
  in
  aux 0 (String.to_list s)

let first_find f_digest idx =
  Iter.init Aoc.identity
  |> Iter.map (fun i -> f_digest (idx + i))
  |> Iter.findi (fun i s ->
         match find_triple s with None -> None | Some x -> Some (idx + i, x))

let second_find f_digest idx c =
  let iter = Iter.init Aoc.identity |> Iter.map (fun i -> f_digest (idx + i + 1)) in
  match
    Iter.take 1000 iter
    |> Iter.findi (fun i s -> if test_penta_char c s then Some i else None)
  with
  | None -> false
  | Some _ -> true

let next_key f_digest idx =
  match first_find f_digest idx with
  | None -> failwith "blah"
  | Some (idx', c) ->
      (* printf "found triple! %c @ %d\n" c idx'; *)
      (second_find f_digest idx' c, idx')

let find_key_idx f_digest =
  let idx = ref 0 in
  let key_idx = ref 0 in
  let count = ref 0 in
  while !count < 64 do
    match next_key f_digest !idx with
    | false, i ->
        (* printf "FAILED\n"; *)
        idx := i + 1
    | true, i ->
        (* printf "KEY FOUND @ %d\n" i; *)
        count := !count + 1;
        key_idx := i;
        idx := i + 1
  done;
  !key_idx

let () =
  printf "part1 = %d\n" (find_key_idx (make_digest "qzyelonm"));
  printf "part2 = %d\n" (find_key_idx (make_digest_part2 "qzyelonm"))
