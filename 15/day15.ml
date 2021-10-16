open Containers
open Format
open Aoc

let data = [ (5, 2); (13, 7); (17, 10); (3, 2); (19, 9); (7, 0) ]

let solve disc_num start_pos num_pos t =
  rem (start_pos + t) num_pos = rem (num_pos - disc_num) num_pos

let solve_eqs t = List.mapi (fun i (p, s) -> solve (i + 1) s p t) data

let solve =
  Iter.init Aoc.identity
  |> Iter.map (fun i -> (i, solve_eqs i))
  |> Iter.filter (fun (_, result) -> List.for_all Aoc.identity result)
  |> Iter.head_exn

let () = printf "%d\n" (fst solve)
