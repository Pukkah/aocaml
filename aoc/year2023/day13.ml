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

let rotate_pattern pattern =
  Util.rotate (pattern |> List.map (fun s -> s |> String.to_seq |> List.of_seq))
  |> List.map (fun l -> l |> List.to_seq |> String.of_seq)
;;

type reflection =
  | Row of int
  | Col of int

let find_reflections (pattern : string list) =
  let rec is_reflection = function
    | [], [] | [], _ | _, [] -> true
    | hd1 :: tl1, hd2 :: tl2 ->
      if String.equal hd1 hd2 then is_reflection (tl1, tl2) else false
  in
  let rec aux2 acc = function
    | [], [] | _, [] -> acc
    | [], hd2 :: tl2 -> aux2 acc ([ hd2 ], tl2)
    | hd1 :: tl1, hd2 :: tl2 ->
      let acc =
        if is_reflection (hd1 :: tl1, hd2 :: tl2)
        then (List.length tl1 + 1) :: acc
        else acc
      in
      aux2 acc (hd2 :: hd1 :: tl1, tl2)
  in
  let rows = aux2 [] ([], pattern) |> List.map (fun x -> Row x) in
  let rotated = rotate_pattern pattern in
  let cols = aux2 [] ([], rotated) |> List.map (fun x -> Col x) in
  rows @ cols
;;

let get_pattern_size pattern = List.length pattern * String.length (List.hd pattern)

let flip_tile n pattern =
  let xlen = List.hd pattern |> String.length in
  let x, y = n mod xlen, n / xlen in
  List.mapi
    (fun i s ->
      if i = y
      then String.mapi (fun j c -> if j = x then if c = '#' then '.' else '#' else c) s
      else s)
    pattern
;;

let part1 patterns =
  List.fold_left
    (fun acc pattern ->
      acc
      +
      match find_reflections pattern |> List.hd with
      | Row n -> n * 100
      | Col n -> n)
    0
    patterns
;;

let part2 patterns =
  let reflections = List.map (fun p -> find_reflections p |> List.hd) patterns in
  List.fold_left2
    (fun acc pattern reflection ->
      let next =
        Array.init (get_pattern_size pattern) (fun i ->
          pattern |> flip_tile i |> find_reflections)
        |> Array.to_list
        |> List.flatten
        |> List.filter (( <> ) reflection)
        |> List.hd
      in
      acc
      +
      match next with
      | Row n -> n * 100
      | Col n -> n)
    0
    patterns
    reflections
;;

let run () =
  print_endline @@ "Part 1: " ^ string_of_int (part1 patterns);
  print_endline @@ "Part 2: " ^ string_of_int (part2 patterns)
;;
