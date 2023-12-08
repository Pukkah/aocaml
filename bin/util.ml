(** Split string on newlines *)
let get_lines str = String.split_on_char '\n' str

(** Greatest common divisor *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b)

(** Least common multiple *)
let lcm a b = a / gcd a b * b
