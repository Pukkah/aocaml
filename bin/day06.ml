let input = "Time:        52     94     75     94\nDistance:   426   1374   1279   1216"

let parse_numbers str =
  String.split_on_char ' ' str
  |> List.map int_of_string_opt
  |> List.filter_map (fun x -> x)
;;

let times, distances =
  let list = Util.get_lines input |> List.map parse_numbers in
  match list with
  | [ times; distances ] -> times, distances
  | _ -> failwith "Invalid input"
;;

let get_winning_count acc time distance =
  let sum = ref 0 in
  for i = 1 to time do
    if i * (time - i) > distance then sum := !sum + 1
  done;
  !sum * acc
;;

let concat_ints ints = String.concat "" (List.map string_of_int ints) |> int_of_string

(* Part 1 *)
let part1 = List.fold_left2 get_winning_count 1 times distances

(* Part 2 *)
let part2 =
  let time = concat_ints times in
  let distance = concat_ints distances in
  get_winning_count 1 time distance
;;

let run () =
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int part2
;;
