open Containers
open Aoc
open Angstrom

type instr = Rect of int * int | RotCol of int * int | RotRow of int * int

(*
 NOT USING THIS ANYMORE.. use ppx_deriving instead!
 
 let string_of_instr = function
  | Rect (r, c) -> Format.sprintf "Rect(%d,%d)" r c
  | RotCol (r, c) -> Format.sprintf "RotCol(%d,%d)" r c
  | RotRow (r, c) -> Format.sprintf "RotRow(%d,%d)" r c 
  *)

let instrp =
  let rectp =
    string "rect " *> U.number >>= fun rows ->
    char 'x' *> U.number <* U.whitespace >>= fun cols -> return (Rect (cols, rows))
  in
  let byp =
    U.number <* string " by " >>= fun x ->
    U.number >>= fun y -> return (x, y)
  in
  let colp = string "rotate column x=" *> byp >>= fun (x, y) -> return (RotCol (x, y)) in
  let rowp = string "rotate row y=" *> byp >>= fun (x, y) -> return (RotRow (x, y)) in
  choice [ rectp; colp; rowp ]

let process_instr b = function
  | Rect (r, c) -> Board.update_region (fun _ -> '#') 0 0 r c b
  | RotRow (row, by) ->
      let x = Board.get_row row b in
      Board.rotate_right ~n:by x;
      Board.set_row row x b
  | RotCol (col, by) ->
      let x = Board.get_col col b in
      Board.rotate_right ~n:by x;
      Board.set_col col x b

let parse l =
  match parse_string ~consume:All instrp l with
  | Ok result -> result
  | Error msg -> failwith msg

let () =
  let data = read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
  let instrs = List.map parse data in
  let b = Board.create_char 6 50 () in
  List.iter (process_instr b) instrs;
  Board.print_char b;
  let count = Board.fold (fun acc x -> if Char.equal x '#' then acc + 1 else acc) 0 b in
  Printf.printf "part1 = %d\n" count;
  Printf.printf "part2 = READ THE LETTERS\n"
