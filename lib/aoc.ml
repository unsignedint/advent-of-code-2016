open Base

(* let hello ?(name="world") () =
  printf "Hello %s!\n" name *)

let read_lines fname =
  Stdio.In_channel.read_lines fname
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
;;

let read_first_line fname = List.hd_exn (read_lines fname)
;;

(* let print_list_rows lst =
  let lst' = lst |> List.map ~f:(Printf.sprintf "%a") in
  List.iter ~f:Stdio.print_endline lst' *)

  (* List.iter ~f:(fun x -> Printf.sprintf Stdio.stdout "%a " x) lst *)

(* let print_list_single lst =
  List.iter ~f:(printf "%d ") *)