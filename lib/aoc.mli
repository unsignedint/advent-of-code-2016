val read_lines : string -> string list

val read_first_line : string -> string

type direction = North | East | South | West

val walk : int * int -> direction -> int * int

val transpose : 'a list list -> 'a list list

module P : sig
  val is_whitespace : char -> bool

  val is_digit : char -> bool

  val is_str : char -> bool

  val is_separator : char -> bool
end

module U : sig
  open Angstrom

  val number : int t

  val word : string t
end

(* module CharH : sig
     type t

     val equal : t -> t -> bool

     val hash : t -> int
   end *)
