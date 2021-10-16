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

let () =
  printf "part1 = %d\n" (fst (solve data1));
  printf "part2 = %d\n" (fst (solve data2))
