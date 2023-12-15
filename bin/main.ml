let () =
  let day = Sys.argv.(1) in
  print_endline ("\nAdvent of Code 2023 - Day " ^ day);
  match int_of_string_opt day with
  | Some 1 -> Day01.run ()
  | Some 2 -> Day02.run ()
  | Some 4 -> Day04.run ()
  | Some 6 -> Day06.run ()
  | Some 7 -> Day07.run ()
  | Some 8 -> Day08.run ()
  | Some 9 -> Day09.run ()
  | Some 10 -> Day10.run ()
  | Some 11 -> Day11.run ()
  | Some 12 -> Day12.run ()
  | Some 13 -> Day13.run ()
  | Some 14 -> Day14.run ()
  | Some 15 -> Day15.run ()
  | Some 16 -> Day16.run ()
  | _ -> failwith "Invalid day"
;;
