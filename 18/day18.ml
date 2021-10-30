open Containers
open Format

let groups3 s =
  let rec aux acc = function
    | [ a; b; c ] -> List.rev ((a, b, c) :: acc)
    | a :: b :: c :: xs -> aux ((a, b, c) :: acc) (b :: c :: xs)
    | _ -> failwith "invalid match?"
  in
  aux [] s

let make_row s =
  let next_c = function
    | '^', '^', '.' | '.', '^', '^' | '^', '.', '.' | '.', '.', '^' -> '^'
    | _, _, _ -> '.'
  in
  let lst = ('.' :: String.to_list s) @ [ '.' ] in
  List.map next_c (groups3 lst) |> String.of_list

let make_rows n s =
  let iter = Iter.iterate make_row s in
  Iter.take n iter

let () =
  let data = Aoc.read_first_line "input.txt" |> String.trim in
  (* let data = ".^^.^.^^^^" in *)
  let bs = String.concat_iter ~sep:"" (make_rows 400000 data) in
  let ht = Aoc.CharHashtblU.from_string bs in
  match Aoc.CharHashtbl.get ht '.' with Some n -> printf "part1 = %d\n" n | None -> ()
