open Advent_of_code

let input = ref None

let read_stdin () =
  input := Some (In_channel.(input_all stdin) |> Util.trim_trailing_newline)
;;

let usage_msg = "aoc <year> <day> [--stdin]"
let speclist = [ "--stdin", Arg.Unit read_stdin, "Read input from stdin" ]

let () =
  Arg.parse speclist (fun _ -> ()) usage_msg;
  let year = int_of_string Sys.argv.(1)
  and day = int_of_string Sys.argv.(2)
  and input = !input in
  let solve_puzzle =
    match year, day with
    (* 2023 *)
    | 2023, 1 -> Year2023.Day01.run ?input
    | 2023, 2 -> Year2023.Day02.run
    | 2023, 4 -> Year2023.Day04.run
    | 2023, 6 -> Year2023.Day06.run
    | 2023, 7 -> Year2023.Day07.run
    | 2023, 8 -> Year2023.Day08.run
    | 2023, 9 -> Year2023.Day09.run
    | 2023, 10 -> Year2023.Day10.run
    | 2023, 11 -> Year2023.Day11.run
    | 2023, 12 -> Year2023.Day12.run
    | 2023, 13 -> Year2023.Day13.run
    | 2023, 14 -> Year2023.Day14.run
    | 2023, 15 -> Year2023.Day15.run
    | 2023, 16 -> Year2023.Day16.run
    | 2023, 19 -> Year2023.Day19.run
    | 2023, 21 -> Year2023.Day21.run
    (* 2016 *)
    | 2016, 1 -> Year2016.Day01.run
    | 2016, 2 -> Year2016.Day02.run
    | 2016, 3 -> Year2016.Day03.run
    | 2016, 4 -> Year2016.Day04.run
    | 2016, 5 -> Year2016.Day05.run
    | 2016, 6 -> Year2016.Day06.run
    | 2016, 7 -> Year2016.Day07.run
    (* 2017 *)
    | 2017, 1 -> Year2017.Day01.run
    | 2017, 2 -> Year2017.Day02.run
    | 2017, 3 -> Year2017.Day03.run
    | 2017, 4 -> Year2017.Day04.run
    | 2017, 5 -> Year2017.Day05.run
    | 2017, 6 -> Year2017.Day06.run
    | 2017, 7 -> Year2017.Day07.run
    | 2017, 8 -> Year2017.Day08.run
    | 2017, 9 -> Year2017.Day09.run
    | 2017, 10 -> Year2017.Day10.run
    | 2017, 11 -> Year2017.Day11.run
    | 2017, 12 -> Year2017.Day12.run
    | 2017, 13 -> Year2017.Day13.run
    | 2017, 14 -> Year2017.Day14.run
    | 2017, 15 -> Year2017.Day15.run
    | 2017, 16 -> Year2017.Day16.run
    | 2017, 18 -> Year2017.Day18.run
    | 2017, 19 -> Year2017.Day19.run
    | 2017, 21 -> Year2017.Day21.run
    | _ -> failwith "not implemented"
  in
  Printf.printf "\nAdvent of Code %i - Day %i\n" year day;
  solve_puzzle ()
;;
