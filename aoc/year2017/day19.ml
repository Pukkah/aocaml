let input' =
  "     |          \n\
  \     |  +--+    \n\
  \     A  |  C    \n\
  \ F---|----E|--+ \n\
  \     |  |  |  D \n\
  \     +B-+  +--+ \n\
  \                "
;;

let parse input =
  input |> Base.String.split_lines |> List.map Base.String.to_array |> Array.of_list
;;

let entrance m = 0, Array.find_index (( = ) '|') m.(0) |> Option.get

let walk m =
  let turn (x, y) (vx, vy) =
    if m.(x + vy).(y + vx) <> ' '
    then (x + vy, y + vx), (vy, vx)
    else (x - vy, y - vx), (-vx, -vy)
  in
  let next (x, y) (vx, vy) =
    if m.(x + vx).(y + vy) = ' ' then turn (x, y) (vx, vy) else (x + vx, y + vy), (vx, vy)
  in
  let rec aux acc steps (x, y) (vx, vy) =
    if m.(x).(y) = ' '
    then acc, steps
    else (
      let pos, dir = next (x, y) (vx, vy) in
      if List.mem m.(x).(y) [ '|'; '-'; '+' ]
      then aux acc (steps + 1) pos dir
      else aux (acc ^ Char.escaped m.(x).(y)) (steps + 1) pos dir)
  in
  aux "" 0 (entrance m) (0, 1)
;;

let run () =
  let part1, part2 = parse input' |> walk in
  part1 |> print_endline;
  part2 |> print_int |> print_newline
;;
