let input =
  "#.##..##.\n\
   ..#.##.#.\n\
   ##......#\n\
   ##......#\n\
   ..#.##.#.\n\
   ..##..##.\n\
   #.#.##.#.\n\n\
   #...##..#\n\
   #....#..#\n\
   ..##..###\n\
   #####.##.\n\
   #####.##.\n\
   ..##..###\n\
   #....#..#"
;;

let patterns =
  let lines = Util.get_lines input |> List.rev in
  let rec aux acc res = function
    | [] -> acc :: res
    | hd :: tl -> if hd = "" then aux [] (acc :: res) tl else aux (hd :: acc) res tl
  in
  aux [] [] lines
;;

let find_mirror_opt (pattern : string list) =
  let rec aux n = function
    | [], [] | [], _ | _, [] -> Some n
    | hd1 :: tl1, hd2 :: tl2 -> if String.equal hd1 hd2 then aux n (tl1, tl2) else None
  in
  let rec aux2 n = function
    | [], [] | [], _ | _, [] -> None
    | hd1 :: tl1, hd2 :: tl2 ->
      (match aux n (hd1 :: tl1, hd2 :: tl2) with
       | Some n -> Some n
       | None -> aux2 (n + 1) (hd2 :: hd1 :: tl1, tl2))
  in
  aux2 1 ([ List.hd pattern ], List.tl pattern)
;;

let rotate_pattern pattern =
  Util.rotate (pattern |> List.map (fun s -> s |> String.to_seq |> List.of_seq))
  |> List.map (fun l -> l |> List.to_seq |> String.of_seq)
;;

let part1 patterns =
  List.map
    (fun pattern ->
      match find_mirror_opt pattern with
      | Some n -> n * 100
      | None ->
        (match find_mirror_opt (rotate_pattern pattern) with
         | Some n -> n
         | None -> 0))
    patterns
  |> List.fold_left ( + ) 0
;;

let run () = print_endline @@ "Part 1: " ^ string_of_int (part1 patterns)
