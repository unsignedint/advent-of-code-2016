open Containers
open Format

let new_part1 size =
  let dq = CCDeque.create () in
  Iter.iter (fun i -> CCDeque.push_back dq i) Iter.Infix.(1 -- size);
  while CCDeque.length dq > 1 do
    CCDeque.(take_front dq |> push_back dq);
    CCDeque.remove_front dq
  done;
  CCDeque.take_front dq

(*
  this was super annoying, but kind of obvious in hindsight;
  use 2 queues to make easy removal at the "mid-point"

  dq1    dq2
  1 2   (3) 4 5   # 3 is removed, 1 rotates
  2      4 5 1    # rebalance by rotating 4
  2 4   (5) 1     # 5 is removed, 2 rotates
  4     (1) 2     # 1 is removed, 4 rotates
         2 4      # rebalace by rotating 2
  2     (4)

  *)
let new_part2 size =
  let dq1 = CCDeque.create () in
  let dq2 = CCDeque.create () in
  let mid_point = size / 2 in
  Iter.iter (fun i -> CCDeque.push_back dq1 i) Iter.Infix.(1 -- mid_point);
  Iter.iter (fun i -> CCDeque.push_back dq2 i) Iter.Infix.(mid_point + 1 -- size);
  while CCDeque.length dq2 > 2 do
    CCDeque.(take_front dq1 |> push_back dq2);
    CCDeque.remove_front dq2;
    if CCDeque.(length dq2 - length dq1) > 1 then
      CCDeque.(take_front dq2 |> push_back dq1)
  done;
  CCDeque.take_front dq1

let () =
  printf "%d\n" (new_part1 3005290);
  printf "%d\n" (new_part2 3005290)
