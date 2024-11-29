(** Utils for Advent of Code *)

let trim_trailing_newline str =
  let len = String.length str in
  if len > 0 && str.[len - 1] = '\n' then String.sub str 0 (len - 1) else str
;;

(** Split string on newlines *)
let get_lines = String.split_on_char '\n'

(** Sum of a list of integers *)
let sum = List.fold_left ( + ) 0

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

(** Rotates a list of N lists of size M into M lists of size N. *)
let rec rotate =
  let rec heads = function
    | [] -> []
    | [] :: _ -> failwith "Invalid list"
    | (x :: _) :: t -> x :: heads t
  in
  let rec tails = function
    | [] -> []
    | [] :: _ -> failwith "Invalid list"
    | (_ :: x) :: t -> x :: tails t
  in
  function
  | [] -> []
  | [] :: t -> rotate t
  | l -> heads l :: rotate (tails l)
;;
