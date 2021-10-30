open Containers
open Angstrom
open Aoc

(* NOTE: removed the `tgl` instruction handling *)

type operand = Reg of char | Val of int [@@deriving show]

type instr =
  | Cpy of operand * operand
  | Inc of char
  | Dec of char
  | Jnz of operand * operand
  (* | Tgl of char *)
  | Out of operand
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
  let outp = lift (fun a -> Out a) (string "out" *> operandp) in
  choice [ cpyp; incp; decp; jnzp; outp ] <?> "invalid instruction"

let parse s =
  (* Format.printf "line = %s\n" s; *)
  match parse_string ~consume:All decoder s with
  | Ok result -> result
  | Error msg -> failwith msg

module CharMap = Map.Make (Char)

let execute instructions registers =
  let update_reg_val k f r = CharMap.add k (f (CharMap.find k r)) r in
  let get_operand r = function Reg a -> CharMap.find a r | Val a -> a in

  (* this will run output is emitted *)
  let rec aux (registers, pc) =
    if pc < 0 || pc >= Array.length instructions then failwith "invalid access"
    else
      (* Printf.printf "%02d %s\n" pc (show_instr instructions.(pc)); *)
      match instructions.(pc) with
      | Cpy (x, d) -> (
          (* ignore the illegal operation *)
          match d with
          | Val _ -> aux (registers, pc + 1)
          | Reg d -> aux (CharMap.add d (get_operand registers x) registers, pc + 1))
      | Inc k -> aux (update_reg_val k (fun x -> x + 1) registers, pc + 1)
      | Dec k -> aux (update_reg_val k (fun x -> x - 1) registers, pc + 1)
      | Jnz (x, os) ->
          if get_operand registers x = 0 then aux (registers, pc + 1)
          else aux (registers, pc + get_operand registers os)
      | Out x -> ((registers, pc + 1), get_operand registers x)
  in
  Iter.fold_map (fun (regs, pc) _ -> aux (regs, pc)) (registers, 0) (Iter.init identity)
  |> Iter.take 12 |> List.of_iter

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
  let instructions = List.map parse data |> Array.of_list in
  let make_regs v = CharMap.(empty |> add 'a' v |> add 'b' 0 |> add 'c' 0 |> add 'd' 0) in
  let quit = ref false in
  let i = ref 0 in
  while not !quit do
    let result = execute (Array.copy instructions) (make_regs !i) in
    Printf.printf "%d - %s\n" !i (String.concat " " (List.map string_of_int result));
    match result with
    | [ 0; 1; 0; 1; 0; 1; 0; 1; 0; 1; 0; 1 ] -> quit := true
    | _ -> i := !i + 1
  done;
  Printf.printf "answer = %d\n" !i
