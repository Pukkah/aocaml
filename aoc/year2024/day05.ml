module SSet = Set.Make (String)

let parse input =
  let lines = Base.String.split_lines input in
  let rec parse_updates acc = function
    | [] -> acc
    | line :: tl ->
      parse_updates ((String.split_on_char ',' line |> List.map int_of_string) :: acc) tl
  and parse_rules acc = function
    | "" :: tl | ([] as tl) -> acc, parse_updates [] tl
    | line :: tl -> parse_rules (SSet.add line acc) tl
  in
  parse_rules SSet.empty lines
;;

let make_pair = Printf.sprintf "%d|%d"

let compare rules x y =
  if SSet.mem (make_pair x y) rules
  then -1
  else if SSet.mem (make_pair y x) rules
  then 1
  else 0
;;

let is_correct rules = Base.List.is_sorted ~compare:(compare rules)
let fix_order rules = List.map (fun e -> List.sort (compare rules) e)
let sum_middles = List.fold_left (fun acc e -> acc + List.nth e (List.length e / 2)) 0

let solve input =
  let rules, updates = parse input in
  let correct, incorrect = List.partition (is_correct rules) updates in
  correct |> sum_middles, incorrect |> fix_order rules |> sum_middles
;;

let test =
  "47|53\n\
   97|13\n\
   97|61\n\
   97|47\n\
   75|29\n\
   61|13\n\
   75|53\n\
   29|13\n\
   97|29\n\
   53|29\n\
   61|53\n\
   97|53\n\
   61|29\n\
   47|13\n\
   75|47\n\
   97|75\n\
   47|61\n\
   75|61\n\
   47|29\n\
   75|13\n\
   53|13\n\n\
   75,47,61,53,29\n\
   97,61,53,29,13\n\
   75,29,13\n\
   75,97,47,61,53\n\
   61,13,29\n\
   97,13,75,29,47"
;;

let%test _ = solve test |> fst = 143
let%test _ = solve test |> snd = 123

let run ?(input = test) () =
  let part1, part2 = solve input in
  part1 |> print_int |> print_newline;
  part2 |> print_int |> print_newline
;;
