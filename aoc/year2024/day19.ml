let parse input =
  let patterns, designs = Base.String.lsplit2_exn ~on:'\n' input in
  ( String.split_on_char ',' patterns |> List.map String.trim
  , Base.String.split_lines designs |> List.tl )
;;

let arrangements_count patterns design =
  let arr = Array.make (String.length design + 1) 0 in
  arr.(0) <- 1;
  for i = 0 to String.length design - 1 do
    if arr.(i) > 0
    then
      List.iter
        (fun substring ->
          let len = String.length substring in
          if i + len <= String.length design
          then (
            let sub = String.sub design i len in
            if sub = substring then arr.(i + len) <- arr.(i + len) + arr.(i)))
        patterns
  done;
  arr.(String.length design)
;;

let part1 input =
  let patterns, designs = parse input in
  Base.List.count designs ~f:(fun design -> arrangements_count patterns design > 0)
;;

let part2 input =
  let patterns, designs = parse input in
  Base.(List.sum (module Int) ~f:(arrangements_count patterns) designs)
;;

let test =
  "r, wr, b, g, bwu, rb, gb, br\n\nbrwrr\nbggr\ngbbr\nrrbgbr\nubwu\nbwurrg\nbrgr\nbbrgwb"
;;

let%test _ = part1 test = 6
let%test _ = part2 test = 16

let run ?(input = test) () =
  part1 input |> print_int |> print_newline;
  part2 input |> print_int |> print_newline
;;
