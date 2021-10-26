open Containers
open Angstrom
open Aoc

type operand = Reg of char | Val of int [@@deriving show]

type instr =
  | Cpy of operand * operand
  | Inc of char
  | Dec of char
  | Jnz of operand * operand
  | Tgl of char
[@@deriving show]

let decoder =
  let operandp =
    U.whitespace
    *> choice [ (U.word >>| fun s -> Reg s.[0]); (U.integer >>| fun n -> Val n) ]
  in
  let regp = U.whitespace *> U.word <* U.whitespace >>= fun s -> return s.[0] in
  let cpyp = lift2 (fun a b -> Cpy (a, b)) (string "cpy" *> operandp) operandp in
  let incp = lift (fun a -> Inc a) (string "inc" *> regp) in
  let decp = lift (fun a -> Dec a) (string "dec" *> regp) in
  let jnzp = lift2 (fun a b -> Jnz (a, b)) (string "jnz" *> operandp) operandp in
  let tglp = lift (fun a -> Tgl a) (string "tgl" *> regp) in
  choice [ cpyp; incp; decp; jnzp; tglp ] <?> "invalid instruction"

let parse s =
  Format.printf "line = %s\n" s;
  match parse_string ~consume:All decoder s with
  | Ok result -> result
  | Error msg -> failwith msg

module CharMap = Map.Make (Char)

let toggle_instr = function
  | Inc x -> Dec x
  | Dec x | Tgl x -> Inc x
  | Jnz (a, b) -> Cpy (a, b)
  | Cpy (a, b) -> Jnz (a, b)

let execute instructions registers =
  let update_reg_val k f r = CharMap.add k (f (CharMap.find k r)) r in
  let get_operand r = function Reg a -> CharMap.find a r | Val a -> a in

  let rec aux registers pc =
    if pc < 0 || pc >= Array.length instructions then registers
    else
      (* Printf.printf "%02d %s\n" pc (show_instr instructions.(pc)); *)
      match instructions.(pc) with
      | Cpy (x, d) -> (
          (* ignore the illegal operation *)
          match d with
          | Val _ -> aux registers (pc + 1)
          | Reg d -> aux (CharMap.add d (get_operand registers x) registers) (pc + 1))
      | Inc k -> aux (update_reg_val k (fun x -> x + 1) registers) (pc + 1)
      | Dec k -> aux (update_reg_val k (fun x -> x - 1) registers) (pc + 1)
      | Jnz (x, os) ->
          if get_operand registers x = 0 then aux registers (pc + 1)
          else aux registers (pc + get_operand registers os)
      | Tgl x ->
          let os = CharMap.find x registers in
          (* need to ignore requests that are out-of-bounds *)
          if pc + os < 0 || pc + os >= Array.length instructions then
            aux registers (pc + 1)
          else
            let instr = instructions.(pc + os) in
            instructions.(pc + os) <- toggle_instr instr;
            aux registers (pc + 1)
  in
  aux registers 0

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
  let instructions = List.map parse data |> Array.of_list in
  let part1 =
    execute (Array.copy instructions)
      CharMap.(empty |> add 'a' 7 |> add 'b' 0 |> add 'c' 0 |> add 'd' 0)
  in
  Printf.printf "part 1 = %d\n" (CharMap.find 'a' part1);
  let part2 =
    execute (Array.copy instructions)
      CharMap.(empty |> add 'a' 12 |> add 'b' 0 |> add 'c' 0 |> add 'd' 0)
  in
  Printf.printf "part 2 = %d\n" (CharMap.find 'a' part2)
