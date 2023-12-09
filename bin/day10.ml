let input =
  "...........\n\
   .S-------7.\n\
   .|F-----7|.\n\
   .||.....||.\n\
   .||.....||.\n\
   .|L-7.F-J|.\n\
   .|..|.|..|.\n\
   .L--J.L--J.\n\
   ..........."
;;

let lines = Util.get_lines input

module Pos = struct
  type t = int * int

  let compare = compare
end

module PosMap = Map.Make (Pos)
module PosSet = Set.Make (Pos)

let pipe_opt x y c =
  match c with
  | 'S' -> Some ((x, y), [ x + 1, y; x, y + 1; x - 1, y; x, y - 1 ])
  | 'J' -> Some ((x, y), [ x, y - 1; x - 1, y ])
  | 'L' -> Some ((x, y), [ x, y - 1; x + 1, y ])
  | '7' -> Some ((x, y), [ x, y + 1; x - 1, y ])
  | 'F' -> Some ((x, y), [ x, y + 1; x + 1, y ])
  | '|' -> Some ((x, y), [ x, y + 1; x, y - 1 ])
  | '-' -> Some ((x, y), [ x + 1, y; x - 1, y ])
  | _ -> None
;;

let pipes =
  List.flatten
  @@ List.mapi
       (fun y line ->
         List.mapi (fun x c -> pipe_opt x y c) @@ List.of_seq @@ String.to_seq line)
       lines
  |> List.filter_map (fun x -> x)
;;

let pipe_map =
  List.fold_left (fun acc (pos, pipe) -> PosMap.add pos pipe acc) PosMap.empty pipes
;;

let start = List.find (fun x -> List.length (snd x) = 4) pipes |> fst
let fist_pos = PosMap.find start pipe_map |> List.find (fun x -> PosMap.mem x pipe_map)

let rec walk step acc curr =
  let next = PosMap.find curr pipe_map |> List.find (fun x -> x <> List.hd acc) in
  if next = start
  then (step / 2) + 1, PosSet.of_list (curr :: acc)
  else walk (step + 1) (curr :: acc) next
;;

let part1, path = walk 0 [ start ] fist_pos

let pipe = function
  | 'J' -> "╯"
  | 'L' -> "╰"
  | '7' -> "╮"
  | 'F' -> "╭"
  | '|' -> "│"
  | '-' -> "─"
  | c -> Char.escaped c
;;

let part2 () =
  let enclosed = ref 0 in
  let last = ref ' ' in
  List.iteri
    (fun y line ->
      let count = ref 0 in
      List.iteri (fun x c ->
        if PosSet.mem (x, y) path
        then (
          (* 'S' -> 'F' is specificly for my input.
             I'm too lazy (it's sunday) calculate it *)
          let c = if c = 'S' then 'F' else c in
          if c = '|' || (c = 'J' && !last = 'F') || (c = '7' && !last = 'L')
          then count := !count + 1;
          if c = 'F' || c = 'L' then last := c;
          print_string (pipe c))
        else if !count mod 2 = 1
        then (
          enclosed := !enclosed + 1;
          print_char 'x')
        else print_char ' ')
      @@ List.of_seq
      @@ String.to_seq line;
      print_newline ())
    lines;
  !enclosed
;;

let run () =
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int (part2 ())
;;
