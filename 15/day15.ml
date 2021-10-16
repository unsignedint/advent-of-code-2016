open Containers
open Format
open Aoc

let data1 = [ (5, 2); (13, 7); (17, 10); (3, 2); (19, 9); (7, 0) ]

let data2 = [ (5, 2); (13, 7); (17, 10); (3, 2); (19, 9); (7, 0); (11, 0) ]

let solve disc_num start_pos num_pos t =
  rem (start_pos + t) num_pos = rem (num_pos - disc_num) num_pos

let solve_eqs data t = List.mapi (fun i (p, s) -> solve (i + 1) s p t) data

let solve data =
  Iter.init Aoc.identity
  |> Iter.map (fun i -> (i, solve_eqs data i))
  |> Iter.filter (fun (_, result) -> List.for_all Aoc.identity result)
  |> Iter.head_exn

(*
CRT version 

CRT is used to solve system of linear congruences of the form =>
x = 2 mod 5
x = 7 mod 13
x = 10 mod 17
...

we can rewrite into the following form =>
x mod 5 = 2
x mod 13 = 7
x mod 17 = 10
...

however, we can't solve this directly, because it will releay the next
cycle that all the equations "align" at their starting position!

we are looking for a specific position, where they are aligned to solve f(t):
  f(t) = (final - start_pos)
  where, final = (num_positions - disc_num) mod num_positions

this pattern is necessary because we need to wait 1 step/tick for each disc
to align, and continue to align

e.g. for 5 positions that is 2 steps down the chain, we want at index 2 so it as 
at index 0 in t+2 steps
T-2 [ . . # . . ]
T-1 [ . # . . . ]
T   [ # . . . . ]  <- yes!

*)

let () =
  let process_input data =
    let n = List.map fst data in
    let a = List.mapi (fun i (npos, start) -> npos - (i + 1) - start) data in
    (n, a)
  in
  let n1, a1 = process_input data1 in
  let n2, a2 = process_input data2 in
  printf "part1 = %d\n" (NumberTheory.crt n1 a1);
  printf "part2 = %d\n" (NumberTheory.crt n2 a2)
