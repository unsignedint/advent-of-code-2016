open Containers
open Angstrom
open Aoc

type ip_fragment = Hyper of string | Addr of string

let hypernetp = char '[' *> U.word <* char ']' >>| fun s -> Hyper s

let addrp = U.word >>| fun s -> Addr s

let parse_ipv7 = many (choice [ hypernetp; addrp ])

let string_of_ip_fragment = function
  | Hyper s -> "Hyper(" ^ s ^ ")"
  | Addr s -> "Addr(" ^ s ^ ")"

let code =
  sep_by (char '-') U.word <* char '-' >>= fun a ->
  U.number >>= fun b ->
  char '[' *> U.word <* char ']' >>= fun c -> return (String.concat "" a, b, c)

let find_abba s =
  let rec aux acc = function
    | a :: b :: c :: d :: xs
      when (not (Char.equal a b)) && Char.equal a d && Char.equal b c ->
        aux (String.of_array [| a; b; c; d |] :: acc) xs
    | _ :: xs -> aux acc xs
    | [] -> acc
  in
  aux [] (String.to_list s)

module StringSet = Set.Make (String)

let set_of_abbas lst = List.map find_abba lst |> List.flatten |> StringSet.of_list

let check_tls (hypers, addrs) =
  let h', a' = (set_of_abbas hypers, set_of_abbas addrs) in
  StringSet.cardinal a' > 0 && StringSet.cardinal h' = 0

let parse_line l =
  let result =
    match parse_string ~consume:All parse_ipv7 l with
    | Ok result -> result
    | Error msg -> failwith msg
  in
  List.partition_filter_map (function Hyper s -> `Left s | Addr s -> `Right s) result

(* part 2 *)
let find_aba s =
  let rec aux acc = function
    | a :: b :: c :: xs when (not (Char.equal a b)) && Char.equal a c ->
        (* note that we support overlap here, rejoin `b` and `c` when recursing *)
        aux (String.of_array [| a; b; c |] :: acc) (b :: c :: xs)
    | _ :: xs -> aux acc xs
    | [] -> acc
  in
  aux [] (String.to_list s)

let set_of_abas lst = List.map find_aba lst |> List.flatten |> StringSet.of_list

let check_ssl (hypers, addrs) =
  let make_bab s = [| s.[1]; s.[0]; s.[1] |] |> String.of_array in
  let h', a' = (set_of_abas hypers, set_of_abas addrs) in
  let result = StringSet.filter (fun s -> StringSet.mem (make_bab s) a') h' in
  StringSet.cardinal result > 0

let () =
  let data = Aoc.read_lines "input.txt" |> List.map String.trim in
  print_endline (String.concat ";" data);
  let supports_tls = List.(map parse_line data |> filter check_tls) in
  Printf.printf "part1 = %d\n" (List.length supports_tls);
  let supports_ssl = List.(map parse_line data |> filter check_ssl) in
  Printf.printf "part2 = %d\n" (List.length supports_ssl)

(* List.iter print_endline (find_abba "abbaiasdfsfsdfoxxo");

   let result =
     match parse_string ~consume:All parse_ipv7 "ioxxojabbar[asdfgh]zxcvbn[jnrj]" with
     | Ok result -> result
     | Error msg -> failwith msg
   in
   List.iter (fun s -> print_endline (string_of_ip_fragment s)) result; *)
(* let hypers, addrs =
     List.partition_filter_map (function Hyper s -> `Left s | Addr s -> `Right s) result
   in
   let hyper_abbas = set_of_abbas hypers in
   let addr_abbas = addrs |> List.map find_abba |> List.flatten |> StringSet.of_list in
   print_endline "HYPER";
   StringSet.iter print_endline hyper_abbas;
   print_endline "ADDR";
   (* print_endline (String.concat "," addr_abbas); *)
   StringSet.iter print_endline addr_abbas;
   if StringSet.(diff addr_abbas hyper_abbas |> cardinal) = StringSet.cardinal addr_abbas
   then print_endline "TLS"
   else print_endline "no TLS" *)

(* List.iter (fun s -> print_endline (string_of_ip_fragment s)) words *)
