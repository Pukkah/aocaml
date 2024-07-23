open Advent_of_code

let () =
  let year = int_of_string Sys.argv.(1)
  and day = int_of_string Sys.argv.(2) in
  Printf.printf "\nAdvent of Code %i - Day %i\n" year day;
  match year, day with
  | 2023, 1 -> Year2023.Day01.run ()
  | 2023, 2 -> Year2023.Day02.run ()
  | 2023, 3 -> Day03.run ()
  | 2023, 4 -> Year2023.Day04.run ()
  | 2023, 6 -> Year2023.Day06.run ()
  | 2023, 7 -> Year2023.Day07.run ()
  | 2023, 8 -> Year2023.Day08.run ()
  | 2023, 9 -> Year2023.Day09.run ()
  | 2023, 10 -> Year2023.Day10.run ()
  | 2023, 11 -> Year2023.Day11.run ()
  | 2023, 12 -> Year2023.Day12.run ()
  | 2023, 13 -> Year2023.Day13.run ()
  | 2023, 14 -> Year2023.Day14.run ()
  | 2023, 15 -> Year2023.Day15.run ()
  | 2023, 16 -> Year2023.Day16.run ()
  | 2023, 17 -> Day17.run ()
  | 2023, 19 -> Year2023.Day19.run ()
  | 2023, 20 -> Day20.run ()
  | 2023, 21 -> Year2023.Day21.run ()
  | 2023, 23 -> Day23.run ()
  | _ -> failwith "not implemented"
;;
