(** Utils for Advent of Code *)

(** Split string on newlines *)
let get_lines = String.split_on_char '\n'

(** Greatest common divisor *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(** Least common multiple *)
let lcm a b = a / gcd a b * b
