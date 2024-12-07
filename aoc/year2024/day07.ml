let test =
  "190: 10 19\n\
   3267: 81 40 27\n\
   83: 17 5\n\
   156: 15 6\n\
   7290: 6 8 6 15\n\
   161011: 16 10 13\n\
   192: 17 8 14\n\
   21037: 9 7 18 13\n\
   292: 11 6 16 20"
;;

let parse input =
  Base.String.split_lines input
  |> List.map (fun line ->
    let res, values = Base.String.rsplit2_exn ~on:':' line in
    int_of_string res, Util.parse_numbers values)
;;

let rec generate_permutations length values =
  if length = 0
  then [ [] ]
  else (
    let shorter_permutations = generate_permutations (length - 1) values in
    List.concat
      (List.map (fun perm -> List.map (fun v -> v :: perm) values) shorter_permutations))
;;

let is_valid ops (res, values) =
  let permutations = generate_permutations (List.length values - 1) ops in
  let rec aux values perm =
    match values, perm with
    | [ acc ], [] -> acc = res
    | acc :: b :: tl, op :: rest -> aux (op acc b :: tl) rest
    | _ -> false
  in
  List.exists (aux values) permutations
;;

let cat a b = int_of_string (Printf.sprintf "%d%d" a b)

let part1 input =
  parse input
  |> List.filter (is_valid [ ( + ); ( * ) ])
  |> List.fold_left (fun acc (res, _) -> acc + res) 0
;;

let part2 input =
  parse input
  |> List.filter (is_valid [ ( + ); ( * ); cat ])
  |> List.fold_left (fun acc (res, _) -> acc + res) 0
;;

let run ?(input = test) () =
  part1 input |> print_int |> print_newline;
  part2 input |> print_int |> print_newline
;;
