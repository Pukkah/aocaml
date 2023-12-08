let input = "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45"

let sequences =
  Util.get_lines input
  |> List.map (fun line -> String.split_on_char ' ' line |> List.map int_of_string)
;;

let predict =
  let rec aux acc = function
    | [ _ ] -> acc
    | a :: b :: tl -> aux ((b - a) :: acc) (b :: tl)
    | _ -> failwith "Invalid input"
  in
  let rec aux2 acc = function
    | list when List.for_all (( = ) 0) list -> Util.sum acc
    | list ->
      let next = aux [] (List.rev list) in
      aux2 (List.hd list :: acc) next
  in
  aux2 []
;;

(* Part1 *)
let part1 = List.map (fun seq -> predict @@ List.rev seq) sequences |> Util.sum

(* Part2 *)
let part2 = List.map predict sequences |> Util.sum

let run () =
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int part2
;;
