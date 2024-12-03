let parse_lists input =
  input
  |> Base.String.split_lines
  |> List.map (fun line ->
    let left = Base.String.lsplit2_exn ~on:' ' line |> fst |> int_of_string
    and right = Base.String.rsplit2_exn ~on:' ' line |> snd |> int_of_string in
    left, right)
  |> Base.List.unzip
;;

let part1 input =
  let left, right = parse_lists input in
  List.fold_left2
    (fun acc left right -> acc + abs (left - right))
    0
    (List.sort ( - ) left)
    (List.sort ( - ) right)
;;

let part2 input =
  let left, right = parse_lists input
  and occurrances = Hashtbl.create 26 in
  List.iter
    (fun right ->
      if Hashtbl.mem occurrances right
      then Hashtbl.replace occurrances right (Hashtbl.find occurrances right + 1)
      else Hashtbl.add occurrances right 1)
    right;
  List.fold_left
    (fun acc left ->
      acc
      +
      match Hashtbl.find_opt occurrances left with
      | Some right -> left * right
      | None -> 0)
    0
    left
;;

let test = "3   4\n4   3\n2   5\n1   3\n3   9\n3   3"
let%test _ = part1 test = 11
let%test _ = part2 test = 31

let run ?(input = test) () =
  part1 input |> print_int |> print_newline;
  part2 input |> print_int |> print_newline
;;
