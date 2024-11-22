let input =
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
;;

let lines = Util.get_lines input

module IntSet = Set.Make (Int)

let parse_card line =
  match List.nth (String.split_on_char ':' line) 1 |> String.split_on_char '|' with
  | [ a; b ] ->
    IntSet.inter
      (IntSet.of_list @@ Util.parse_numbers a)
      (IntSet.of_list @@ Util.parse_numbers b)
    |> IntSet.cardinal
  | _ -> failwith "Invalid card"
;;

let score x = if x > 0 then Int.shift_left 1 (x - 1) else 0
let cards = List.map parse_card lines

(* Part 1 *)
let part1 = List.fold_left (fun acc x -> acc + score x) 0 cards

(* Part 2 *)
let part2 =
  let arr = Array.init (List.length cards) (fun _ -> ref 1) in
  List.mapi
    (fun i x ->
      let tickets = !(arr.(i)) in
      for j = 1 to x do
        let k = arr.(i + j) in
        k := !k + tickets
      done;
      tickets)
    cards
  |> List.fold_left ( + ) 0
;;

let run () =
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int part2
;;
