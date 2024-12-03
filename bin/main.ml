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
    (* 2024 *)
    | 2024, 1 -> Year2024.Day01.run ?input
    | 2024, 3 -> Year2024.Day03.run ?input
    | _ -> failwith "not implemented"
  in
  Printf.printf "\nAdvent of Code %i - Day %i\n" year day;
  solve_puzzle ()
;;
