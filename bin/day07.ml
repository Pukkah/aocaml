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

let get_type hand =
  match List.sort ( - ) hand with
  (* five of a kind *)
  | [ a; b; c; d; e ] when a = b && b = c && c = d && d = e -> 6
  (* four of a kind *)
  | [ a; b; c; d; e ] when (a = b && b = c && c = d) || (b = c && c = d && d = e) -> 5
  (* full house *)
  | [ a; b; c; d; e ] when (a = b && b = c && d = e) || (a = b && c = d && d = e) -> 4
  (* three of a kind *)
  | [ a; b; c; d; e ] when (a = b && b = c) || (b = c && c = d) || (c = d && d = e) -> 3
  (* two pairs *)
  | [ a; b; c; d; e ] when (a = b && c = d) || (a = b && d = e) || (b = c && d = e) -> 2
  (* one pair *)
  | [ a; b; c; d; e ] when a = b || b = c || c = d || d = e -> 1
  (* high card *)
  | _ -> 0
;;

let rec tiebreak hand1 hand2 =
  match hand1, hand2 with
  | [], [] -> 0
  | a :: ta, b :: tb when a = b -> tiebreak ta tb
  | a :: _, b :: _ -> if a > b then 1 else -1
  | _ -> failwith "invalid input"
;;

let compare (hand1, _) (hand2, _) =
  match get_type hand1, get_type hand2 with
  | a, b when a > b -> 1
  | a, b when a < b -> -1
  | _ -> tiebreak hand1 hand2
;;

let part1 =
  hands
  |> List.sort compare
  |> List.mapi (fun i (_, score) -> score * (i + 1))
  |> List.fold_left ( + ) 0
;;

let run () = print_endline @@ string_of_int part1
