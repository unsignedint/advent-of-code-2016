open Containers

let read_all_lines fname = IO.(with_in fname read_lines_l)

let read_lines fname =
  read_all_lines fname |> List.map String.trim
  |> List.filter (fun s -> not (String.is_empty s))

let read_first_line fname = List.hd (read_lines fname)

type direction = North | East | South | West

let walk (x, y) = function
  | North -> (x, y + 1)
  | South -> (x, y - 1)
  | East -> (x + 1, y)
  | West -> (x - 1, y)

let rec transpose m =
  assert (not (List.is_empty m));
  if List.mem [] m then [] else List.map List.hd m :: transpose (List.map List.tl m)

(* parsing *)
module P = struct
  let is_whitespace = function '\x20' | '\x0a' | '\x0d' | '\x09' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_str = function 'a' .. 'z' -> true | _ -> false

  let is_separator = function '-' | '_' -> true | _ -> false
end

module U = struct
  open Angstrom

  let number = take_while1 P.is_digit >>| int_of_string

  let word = take_while1 P.is_str
end

(* module CharH = struct
     type t = char

     let equal x y = Char.code x = Char.code y

     let hash = Char.code
   end *)
