open Containers
open Angstrom
open Aoc

type operand = Reg of char | Val of int [@@deriving show]

type instr = Cpy of operand * char | Inc of char | Dec of char | Jnz of operand * int
[@@deriving show]

let decoder =
  let operandp =
    U.whitespace
    *> choice [ (U.word >>| fun s -> Reg s.[0]); (U.number >>| fun n -> Val n) ]
  in
  let regp = U.whitespace *> U.word <* U.whitespace >>= fun s -> return s.[0] in
  let cpyp = lift2 (fun a b -> Cpy (a, b)) (string "cpy" *> operandp) regp in
  let incp = lift (fun a -> Inc a) (string "inc" *> regp) in
  let decp = lift (fun a -> Dec a) (string "dec" *> regp) in
  let jnzp = lift2 (fun a b -> Jnz (a, b)) (string "jnz" *> operandp) U.integer in
  choice [ cpyp; incp; decp; jnzp ]

let parse s =
  match parse_string ~consume:All decoder s with
  | Ok result -> result
  | Error msg -> failwith msg

module CharMap = Map.Make (Char)

let execute instructions registers =
  let update_reg_val k f r = CharMap.add k (f (CharMap.find k r)) r in
  let get_operand = function Reg a -> CharMap.find a registers | Val a -> a in

  let rec aux registers pc =
    if pc < 0 || pc >= Array.length instructions then registers
    else
      (* Printf.printf "%02d %s\n" pc (show_instr instructions.(pc)); *)
      match instructions.(pc) with
      | Cpy (x, d) -> aux (CharMap.add d (get_operand x) registers) (pc + 1)
      | Inc k -> aux (update_reg_val k (fun x -> x + 1) registers) (pc + 1)
      | Dec k -> aux (update_reg_val k (fun x -> x - 1) registers) (pc + 1)
      | Jnz (x, os) ->
          if get_operand x = 0 then aux registers (pc + 1) else aux registers (pc + os)
  in
  aux registers 0

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
  let instructions = List.map parse data |> Array.of_list in
  (* Array.iter (fun instr -> print_endline (show_instr instr)) instructions; *)
  let part1 =
    execute instructions
      CharMap.(empty |> add 'a' 0 |> add 'b' 0 |> add 'c' 0 |> add 'd' 0)
  in
  (* CharMap.iter (fun k v -> Printf.printf "register %c - %d\n" k v) part1; *)
  Printf.printf "part 1 = %d\n" (CharMap.find 'a' part1);
  let part2 =
    execute instructions
      CharMap.(empty |> add 'a' 0 |> add 'b' 0 |> add 'c' 1 |> add 'd' 0)
  in
  Printf.printf "part 2 = %d\n" (CharMap.find 'a' part2)
