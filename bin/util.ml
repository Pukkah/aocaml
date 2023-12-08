(** Utils for Advent of Code *)

(** Split string on newlines *)
let get_lines = String.split_on_char '\n'

(** Slits string on whitespace and extracts integers *)
let parse_numbers str =
  String.split_on_char ' ' str
  |> List.map int_of_string_opt
  |> List.filter_map (fun x -> x)
;;

(** Greatest common divisor *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(** Least common multiple *)
let lcm a b = a / gcd a b * b
