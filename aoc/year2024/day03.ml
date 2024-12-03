let part1 input =
  let pattern = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let rec find_matches pos acc =
    try
      let _ = Str.search_forward pattern input pos in
      let a = int_of_string (Str.matched_group 1 input) in
      let b = int_of_string (Str.matched_group 2 input) in
      find_matches (Str.match_end ()) ((a * b) + acc)
    with
    | Not_found -> acc
  in
  find_matches 0 0
;;

let part2 input =
  let pattern = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))\|do()\|don't()|} in
  let rec find_matches pos acc mul =
    try
      let _ = Str.search_forward pattern input pos in
      let matched = Str.matched_group 0 input in
      if String.starts_with ~prefix:"mul" matched && mul
      then (
        let a = int_of_string (Str.matched_group 1 input) in
        let b = int_of_string (Str.matched_group 2 input) in
        find_matches (Str.match_end ()) ((a * b) + acc) mul)
      else find_matches (Str.match_end ()) acc (matched = "do()")
    with
    | Not_found -> acc
  in
  find_matches 0 0 true
;;

let test1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
let%test _ = part1 test1 = 161
let test2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
let%test _ = part2 test2 = 48

let run ?input () =
  let input1, input2 =
    match input with
    | Some x -> x, x
    | None -> test1, test2
  in
  part1 input1 |> print_int |> print_newline;
  part2 input2 |> print_int |> print_newline
;;
