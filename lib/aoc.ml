open Containers

let identity x = x

(**
  [rem] calculates a typical Python-style modulo operation: [rem a b] == [a % b]
*)
let rem x y =
  let result = x mod y in
  if result >= 0 then result else result + y

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

(* https://rosettacode.org/wiki/Matrix_transposition#OCaml *)
let rec transpose m =
  assert (not (List.is_empty m));
  if List.mem [] m then [] else List.map List.hd m :: transpose (List.map List.tl m)

(* parsing *)
module P = struct
  let is_whitespace = function '\x20' | '\x0a' | '\x0d' | '\x09' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_str = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let is_separator = function '-' | '_' -> true | _ -> false
end

module U = struct
  open Angstrom

  let whitespace = take_while P.is_whitespace

  let number = take_while1 P.is_digit >>| int_of_string

  let integer =
    lift2
      (fun a b -> a * b)
      (whitespace *> peek_char_fail >>= function
       | '-' -> advance 1 >>| fun () -> -1
       | _ -> return 1)
      number

  let word = take_while1 P.is_str

  let until_eol = take_till (function '\n' | '\r' -> true | _ -> false)
end

module CharHashtbl = CCHashtbl.Make (struct
  type t = char

  let equal x y = Char.code x = Char.code y

  let hash = Char.code
end)

module CharHashtblU = struct
  (* sort first by frequency descending, then key ascending *)
  let sorted_v_k (h : 'a CharHashtbl.t) =
    CharHashtbl.to_list h
    |> List.sort (fun (k1, v1) (k2, v2) ->
           match Int.compare v2 v1 with 0 -> Char.compare k1 k2 | c -> c)

  let from_string s =
    let h = CharHashtbl.create (String.length s) in
    List.iter (fun c -> CharHashtbl.incr ~by:1 h c) (String.to_list s);
    h
end

let list_of_pair (a, b) = [ a; b ]

let pair_of_list = function [ a; b ] -> (a, b) | _ -> failwith "invalid list"

let rotate_left ?(n = 1) a =
  let n = rem n (Array.length a) in
  let b = Array.copy a in
  Array.blit b n a 0 (Array.length a - n);
  Array.blit b 0 a (Array.length a - n) n

let rotate_right ?(n = 1) a =
  let n = rem n (Array.length a) in
  let b = Array.copy a in
  Array.blit b (Array.length a - n) a 0 n;
  Array.blit b 0 a n (Array.length a - n)

(* 2D board operations *)
module Board = struct
  open Bigarray

  let create_int rows cols ?(f = fun i j -> (i * cols) + j) () =
    Array2.init Int8_unsigned c_layout rows cols f

  let create_char rows cols ?(c = '.') () =
    Array2.init Bigarray.Char c_layout rows cols (fun _ _ -> c)

  let print_int a =
    for i = 0 to Array2.dim1 a - 1 do
      for j = 0 to Array2.dim2 a - 1 do
        Printf.printf "%02d " a.{i, j}
      done;
      print_endline ""
    done

  let print_char a =
    for i = 0 to Array2.dim1 a - 1 do
      for j = 0 to Array2.dim2 a - 1 do
        Printf.printf "%c" a.{i, j}
      done;
      print_endline ""
    done

  let get_row row a = Array.init (Array2.dim2 a) (fun j -> a.{row, j})

  let get_col col a = Array.init (Array2.dim1 a) (fun i -> a.{i, col})

  let set_row row data a =
    for j = 0 to Array2.dim2 a - 1 do
      a.{row, j} <- data.(j)
    done

  let set_col col data a =
    for i = 0 to Array2.dim1 a - 1 do
      a.{i, col} <- data.(i)
    done

  let rotate_left = rotate_left

  let rotate_right = rotate_right

  (*
     let rotate_left a =
       let dim = Array.length a in
       let temp = a.(0) in
       for i = 1 to dim - 1 do
         a.(i - 1) <- a.(i)
       done;
       a.(dim - 1) <- temp

     let rotate_right a =
       let dim = Array.length a in
       let temp = a.(dim - 1) in
       for i = dim - 1 downto 1 do
         a.(i) <- a.(i - 1)
       done;
       a.(0) <- temp *)

  let update_region f x y num_rows num_cols a =
    for i = 0 to num_rows - 1 do
      for j = 0 to num_cols - 1 do
        a.{y + i, x + j} <- f a.{y + i, x + j}
      done
    done

  let fold f init a =
    let ii = Iter.Infix.(0 -- (Array2.dim1 a - 1)) in
    let jj = Iter.Infix.(0 -- (Array2.dim2 a - 1)) in
    Iter.fold (fun acc i -> Iter.fold (fun acc j -> f acc a.{i, j}) acc jj) init ii
end

let repeat f n = Iter.iter f Iter.Infix.(1 -- n)

let repeat_fold f init n = Iter.fold f init Iter.Infix.(1 -- n)

let rec extract k list =
  if k <= 0 then [ [] ]
  else
    match list with
    | [] -> []
    | h :: tl ->
        let with_h = List.map (fun l -> h :: l) (extract (k - 1) tl) in
        let without_h = extract k tl in
        with_h @ without_h

module MakeBFS (O : Map.OrderedType) = struct
  module SeenMap = Map.Make (O)

  (**
    [bfs] performs a breadth-first search, starting from [initial_state] until the 
    condition [f_end_condition state] is true; and a pair of [state * seen] is returned.

    [f_seen_kv state] maps to a key and value to set in the seen map to prune search
    [f_make_candidates state] should return a new list of candidate states to try
  *)
  let bfs ~initial_state ~f_end_condition ~f_seen_kv ~f_make_candidates =
    let rec aux seen q =
      let curr_state, q = CCSimple_queue.pop_exn q in
      let k, v = f_seen_kv curr_state in
      (* skip if we've already seen this! *)
      if SeenMap.mem k seen then aux seen q (* end if we've found our solution *)
      else if f_end_condition curr_state then (curr_state, seen)
      else
        (* add to seen set *)
        let seen' = SeenMap.add k v seen in
        (* add new frontier to queue *)
        let q' =
          f_make_candidates curr_state
          |> List.fold_left (fun acc candidate -> CCSimple_queue.push candidate acc) q
        in
        aux seen' q'
    in
    aux SeenMap.empty CCSimple_queue.(empty |> push initial_state)
end

(**
  [memoise] will cache previous executions in a _Hashtbl_. Use the [init] parameter
  to set starting capacity of the structure.

  _NOTE_ will only work with a single parameter function. Write a wrapper to handle
  additional parameters, if required.
*)
let memoise f =
  let h = Hashtbl.create 10000 in
  fun x ->
    try Hashtbl.find h x
    with Not_found ->
      let y = f x in
      Hashtbl.add h x y;
      y

module NumberTheory = struct
  (* implementation ported from https://rosettacode.org/wiki/Chinese_remainder_theorem#Procedural *)
  let mul_inv a' b' =
    let a = ref a' in
    let b = ref b' in
    let b0 = b' in
    let x0, x1 = (ref 0, ref 1) in
    while !a > 1 do
      let q = !a / !b in
      let tmp = rem !a !b in
      a := !b;
      b := tmp;
      let tmp = !x0 in
      x0 := !x1 - (q * !x0);
      x1 := tmp
    done;
    if !x1 < 0 then x1 := !x1 + b0;
    !x1

  (**
    [crt n a] solves the Chinese Remainder Theorem for the set of equations:
      x = a1 mod n1, x = a2 mod n2, ... x = ai mod ni

    For example, to solve x in: x mod 4 = 3 and x mod 5 = 4
    [crt [4;5] [3;4] = 19]
    *)
  let crt n a =
    let prod = List.reduce_exn (fun a b -> a * b) n in
    let zipped = List.combine n a in
    let sum =
      List.fold_left
        (fun acc (ni, ai) ->
          let p = prod / ni in
          acc + (ai * mul_inv p ni * p))
        0 zipped
    in
    rem sum prod
end

(* c.f. https://github.com/ocaml-batteries-team/batteries-included/pull/513 *)
let permutations l =
  (* do a choice in [l]. [right] contain elements not to choose from. *)
  let rec choose_first among right =
    match among with
    | [] -> List.cons [] List.empty
    | [ x ] -> perms_starting_with x right
    | x :: among' ->
        (* choose [x], or don't (in which case put it in [right]) *)
        List.append
          (perms_starting_with x (among' @ right))
          (choose_first among' (x :: right))
  (* all permutations of [l], prefixed with [x] *)
  and perms_starting_with x l = List.map (fun l -> x :: l) (choose_first l []) in
  choose_first l []

(* c.f. https://github.com/ocaml-batteries-team/batteries-included/pull/513 *)
let combinations l =
  let rec gen l =
    match l with
    | [] -> List.cons [] List.empty
    | x :: l' ->
        let tl = gen l' in
        let node = List.append tl (List.map (fun l -> x :: l) tl) in
        node
  in
  gen l

module Coord = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) = match compare x0 x1 with 0 -> compare y0 y1 | c -> c
end
