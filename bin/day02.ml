let input =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
;;

let lines = Util.get_lines input
let extract_id str = int_of_string @@ List.nth (String.split_on_char ' ' str) 1

type color =
  | Red of int
  | Green of int
  | Blue of int

let parse_cube str =
  match String.split_on_char ' ' @@ String.trim str with
  | [ n; "red" ] -> Red (int_of_string n)
  | [ n; "green" ] -> Green (int_of_string n)
  | [ n; "blue" ] -> Blue (int_of_string n)
  | _ -> failwith "Invalid cube"
;;

let colors_to_tuple colors =
  let rec aux (r, g, b) = function
    | [] -> Red r, Green g, Blue b
    | Red n :: tl -> aux (n, g, b) tl
    | Green n :: tl -> aux (r, n, b) tl
    | Blue n :: tl -> aux (r, g, n) tl
  in
  aux (0, 0, 0) colors
;;

let parse_round str =
  String.split_on_char ',' str |> List.map parse_cube |> colors_to_tuple
;;

let parse_rounds str = String.split_on_char ';' str |> List.map parse_round

let parse_game line =
  match String.split_on_char ':' line with
  | [ id; rounds ] -> extract_id id, parse_rounds rounds
  | _ -> failwith "Invalid game"
;;

let games = List.map parse_game lines
let max_r, max_g, max_b = Red 12, Green 13, Blue 14

(* Part 1 *)
let part1 =
  List.filter
    (fun (_, rounds) ->
      List.exists (fun (r, g, b) -> r > max_r || g > max_g || b > max_b) rounds |> not)
    games
  |> List.fold_left (fun acc (id, _) -> acc + id) 0
;;

let run () = print_endline @@ "Part 1: " ^ string_of_int part1
