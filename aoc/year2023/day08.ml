let input = "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
let lines = Util.get_lines input

let instructions =
  List.hd lines |> String.to_seq |> Array.of_seq |> Array.map (fun c -> c = 'R')
;;

let len = Array.length instructions

module StringMap = Map.Make (String)

let parse_maze lines =
  let rec aux map = function
    | [] -> map
    | line :: tl ->
      let key = String.sub line 0 3 in
      let left = String.sub line 7 3 in
      let right = String.sub line 12 3 in
      let map = StringMap.add key (left, right) map in
      aux map tl
  in
  aux StringMap.empty lines
;;

let maze =
  match lines with
  | _ :: _ :: tl -> parse_maze tl
  | _ -> failwith "Invalid input"
;;

let ghosts = StringMap.bindings maze |> List.map fst |> List.filter (fun s -> s.[2] = 'A')

let solve =
  let rec aux step = function
    | s when s.[2] = 'Z' -> step
    | elem ->
      let l, r = StringMap.find elem maze in
      let next = if instructions.(step mod len) then r else l in
      aux (step + 1) next
  in
  aux 0
;;

(* Part 1 *)
let part1 = solve "AAA"

(* Part 2 *)
let part2 = List.map solve ghosts |> List.fold_left Util.lcm 1

let run () =
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int part2
;;
