let input = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"

let parse_cards hand =
  String.to_seq hand
  |> List.of_seq
  |> List.map (fun c ->
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> 11
    | 'T' -> 10
    | _ -> int_of_string @@ Char.escaped c)
;;

let hands =
  Util.get_lines input
  |> List.map (fun line ->
    match String.split_on_char ' ' line with
    | [ hand; score ] -> parse_cards hand, int_of_string score
    | _ -> failwith "invalid input")
;;

module CardMap = Map.Make (Int)

let count_cards list =
  let add_card count_map card =
    let count =
      match CardMap.find_opt card count_map with
      | Some c -> c + 1
      | None -> 1
    in
    CardMap.add card count count_map
  in
  List.fold_left add_card CardMap.empty list
;;

let joker = 1

let get_rank hand =
  let counted = count_cards hand in
  let jokers =
    match CardMap.find_opt joker counted with
    | Some c -> c
    | None -> 0
  in
  let cleaned = CardMap.remove joker counted |> CardMap.bindings |> List.map snd in
  match jokers with
  | 5 -> 9
  | _ ->
    let unique = List.length cleaned in
    let high = List.sort ( - ) cleaned |> List.rev |> List.hd in
    5 - unique + high + jokers
;;

let rec tiebreak hand1 hand2 =
  match hand1, hand2 with
  | [], [] -> 0
  | a :: ta, b :: tb when a = b -> tiebreak ta tb
  | a :: _, b :: _ -> if a > b then 1 else -1
  | _ -> failwith "invalid input"
;;

let compare (hand1, _) (hand2, _) =
  match get_rank hand1, get_rank hand2 with
  | a, b when a > b -> 1
  | a, b when a < b -> -1
  | _ -> tiebreak hand1 hand2
;;

let solve list =
  list
  |> List.sort compare
  |> List.mapi (fun i x -> snd x * (i + 1))
  |> List.fold_left ( + ) 0
;;

(* Part 1 *)
let part1 = solve hands

(* Part 2 *)
let part2 =
  List.map
    (fun (hand, score) ->
      List.map (fun card -> if card = 11 then joker else card) hand, score)
    hands
  |> solve
;;

let run () =
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int part2
;;
