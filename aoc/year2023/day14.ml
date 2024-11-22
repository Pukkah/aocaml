let input =
  "O....#....\n\
   O.OO#....#\n\
   .....##...\n\
   OO.#O....O\n\
   .O.....O#.\n\
   O.#..O.#.#\n\
   ..O..#O..O\n\
   .......O..\n\
   #....###..\n\
   #OO..#...."
;;

let parse_input input =
  input
  |> Util.get_lines
  |> List.map (fun line -> line |> String.to_seq |> List.of_seq)
  |> Util.rotate
;;

let move_rocks line =
  let rec move_rocks' free acc = function
    | [] -> List.rev (List.sort compare free @ acc)
    | '#' :: tl -> move_rocks' [] ('#' :: (List.sort compare free @ acc)) tl
    | h :: tl -> move_rocks' (h :: free) acc tl
  in
  move_rocks' [] [] line
;;

let total_load platform =
  let size = List.length platform in
  platform
  |> Util.rotate
  |> List.mapi (fun i line -> (size - i) * (List.filter (( = ) 'O') line |> List.length))
  |> List.fold_left ( + ) 0
;;

let cycle platform =
  let rec cycle' plat = function
    | 0 -> plat
    | n -> cycle' (List.map move_rocks plat |> Util.rotate |> List.rev) (n - 1)
  in
  cycle' platform 4
;;

let hash platform =
  platform
  |> List.map (fun line -> List.to_seq line |> String.of_seq)
  |> String.concat "\n"
;;

module StrMap = Map.Make (String)
module IntMap = Map.Make (Int)

let spin platform max_cycles =
  let seen = ref StrMap.empty in
  let scores = ref IntMap.empty in
  let rec spin' platform' cycle' =
    match StrMap.find_opt (hash platform') !seen with
    | Some cycle ->
      let index = cycle + ((max_cycles - cycle) mod (cycle' - cycle)) in
      IntMap.find index !scores
    | None ->
      seen := StrMap.add (hash platform') cycle' !seen;
      scores := IntMap.add cycle' (total_load platform') !scores;
      spin' (cycle platform') (cycle' + 1)
  in
  spin' platform 0
;;

let part1 platform = platform |> List.map move_rocks |> total_load
let part2 platform = spin platform 1_000_000_000

let run () =
  let platform = parse_input input in
  print_endline @@ "Part 1: " ^ (part1 platform |> string_of_int);
  print_endline @@ "Part 2: " ^ (part2 platform |> string_of_int)
;;
