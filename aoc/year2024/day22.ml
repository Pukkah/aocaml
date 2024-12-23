let parse input = Base.String.split_lines input |> List.map int_of_string
let mix sv v = v lxor sv
let prune v = v mod 16777216

let next secret =
  let secret = secret * 64 |> mix secret |> prune in
  let secret = secret / 32 |> mix secret |> prune in
  let secret = secret * 2048 |> mix secret |> prune in
  secret
;;

let rec calculate_nth_value ~f current ~n =
  if n = 0 then current else calculate_nth_value ~f (f current) ~n:(n - 1)
;;

let part1 input =
  let seeds = parse input in
  Base.List.sum (module Base.Int) ~f:(calculate_nth_value ~f:next ~n:2000) seeds
;;

let test = "1\n10\n100\n2024"
let%test _ = part1 test = 37327623
let run ?(input = test) () = part1 input |> print_int |> print_newline
