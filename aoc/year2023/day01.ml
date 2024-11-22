let input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

let input2 =
  "two1nine\n\
   eightwothree\n\
   abcone2threexyz\n\
   xtwone3four\n\
   4nineeightseven2\n\
   zoneight234\n\
   7pqrstsixteen"
;;

let get_value str =
  let rec aux = function
    | ('0' .. '9' as c) :: _ -> Char.escaped c
    | _ :: tl -> aux tl
    | [] -> raise Not_found
  in
  let list = String.to_seq str |> List.of_seq in
  int_of_string @@ aux list ^ aux (List.rev list)
;;

let get_value_literal str =
  let rec aux acc list =
    match list with
    | [] -> acc
    | ('1' .. '9' as c) :: tl -> aux (int_of_string (Char.escaped c) :: acc) tl
    | 'o' :: ('n' :: 'e' :: _ as tl) -> aux (1 :: acc) tl
    | 't' :: ('w' :: 'o' :: _ as tl) -> aux (2 :: acc) tl
    | 't' :: ('h' :: 'r' :: 'e' :: 'e' :: _ as tl) -> aux (3 :: acc) tl
    | 'f' :: ('o' :: 'u' :: 'r' :: _ as tl) -> aux (4 :: acc) tl
    | 'f' :: ('i' :: 'v' :: 'e' :: _ as tl) -> aux (5 :: acc) tl
    | 's' :: ('i' :: 'x' :: _ as tl) -> aux (6 :: acc) tl
    | 's' :: ('e' :: 'v' :: 'e' :: 'n' :: _ as tl) -> aux (7 :: acc) tl
    | 'e' :: ('i' :: 'g' :: 'h' :: 't' :: _ as tl) -> aux (8 :: acc) tl
    | 'n' :: ('i' :: 'n' :: 'e' :: _ as tl) -> aux (9 :: acc) tl
    | _ :: tl -> aux acc tl
  in
  let digits = aux [] (String.to_seq str |> List.of_seq) in
  let first = List.hd (List.rev digits) in
  let last = List.hd digits in
  int_of_string @@ string_of_int first ^ string_of_int last
;;

let solve_puzzle parser input =
  let lines = Util.get_lines input in
  List.map parser lines |> List.fold_left ( + ) 0
;;

(*  Part1 *)
let part1 = solve_puzzle get_value input

(*  Part2 *)
let part2 = solve_puzzle get_value_literal input2

let run () =
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int part2
;;
