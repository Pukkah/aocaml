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
  input |> Util.get_lines |> List.map (fun line -> line |> String.to_seq |> List.of_seq)
;;

let move_rocks line =
  let rec move_rocks' free acc = function
    | [] -> List.rev (List.sort compare free @ acc)
    | '#' :: tl -> move_rocks' [] ('#' :: (List.sort compare free @ acc)) tl
    | h :: tl -> move_rocks' (h :: free) acc tl
  in
  move_rocks' [] [] line
;;

let part1 platform =
  let size = List.length platform in
  platform
  |> Util.rotate
  |> List.map move_rocks
  |> Util.rotate
  |> List.mapi (fun i line -> (size - i) * (List.filter (( = ) 'O') line |> List.length))
  |> List.fold_left ( + ) 0
;;

let run () =
  let platform = parse_input input in
  print_endline @@ "Part 1: " ^ (part1 platform |> string_of_int)
;;
