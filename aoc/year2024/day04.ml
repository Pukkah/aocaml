let parse input = Array.of_list Base.String.(split_lines input |> List.map to_array)
let term = String.to_seqi "XMAS"
let directions = [ -1, 0; -1, 1; 0, 1; 1, 1; 1, 0; 1, -1; 0, -1; -1, -1 ]

let xmas arr x y =
  if arr.(x).(y) <> 'X'
  then 0
  else (
    let term_len = Seq.length term in
    let cases =
      List.filter_map
        (fun (dx, dy) ->
          if (dx = -1 && x < term_len - 1)
             || (dx = 1 && x + term_len > Array.length arr)
             || (dy = -1 && y < term_len - 1)
             || (dy = 1 && y + term_len > Array.length arr.(x))
          then None
          else Some (dx, dy))
        directions
    in
    List.fold_left
      (fun acc (dx, dy) ->
        acc
        +
        if Seq.for_all (fun (i, c) -> c = arr.((i * dx) + x).((i * dy) + y)) term
        then 1
        else 0)
      0
      cases)
;;

let part1 input =
  let arr = parse input
  and count = ref 0 in
  for x = 0 to Array.length arr - 1 do
    for y = 0 to Array.length arr.(x) - 1 do
      count := !count + xmas arr x y
    done
  done;
  !count
;;

let part2 input =
  let arr = parse input
  and count = ref 0 in
  for x = 1 to Array.length arr - 2 do
    for y = 1 to Array.length arr.(x) - 2 do
      if arr.(x).(y) = 'A'
      then (
        let around =
          [ arr.(x - 1).(y - 1)
          ; arr.(x - 1).(y + 1)
          ; arr.(x + 1).(y - 1)
          ; arr.(x + 1).(y + 1)
          ]
        in
        if List.fold_left (fun acc c -> acc + if c = 'M' then 1 else 0) 0 around = 2
           && List.fold_left (fun acc c -> acc + if c = 'S' then 1 else 0) 0 around = 2
           && arr.(x - 1).(y - 1) <> arr.(x + 1).(y + 1)
        then incr count)
    done
  done;
  !count
;;

let test =
  "MMMSXXMASM\n\
   MSAMXMSMSA\n\
   AMXSXMAAMM\n\
   MSAMASMSMX\n\
   XMASAMXAMM\n\
   XXAMMXXAMA\n\
   SMSMSASXSS\n\
   SAXAMASAAA\n\
   MAMMMXMMMM\n\
   MXMXAXMASX"
;;

let%test _ = part1 test = 18
let%test _ = part2 test = 9

let run ?(input = test) () =
  part1 input |> print_int |> print_newline;
  part2 input |> print_int |> print_newline
;;
