let input =
  "FF7FSF7F7F7F7F7F---7\n\
   L|LJ||||||||||||F--J\n\
   FL-7LJLJ||||||LJL-77\n\
   F--JF--7||LJLJ7F7FJ-\n\
   L---JF-JLJ.||-FJLJJ7\n\
   |F|F-JF---7F7-L7L|7|\n\
   |FFJF7L7F-JF7|JL---7\n\
   7-L-JL7||F7|L7F-7F7|\n\
   L.L7LFJ|||||FJL7||LJ\n\
   L7JLJL-JLJLJL--JLJ.L"
;;

let lines = Util.get_lines input

module Pos = struct
  type t = int * int

  let sub (x1, y1) (x2, y2) = x2 - x1, y2 - y1
  let add (x1, y1) (x2, y2) = x2 + x1, y2 + y1
  let compare = compare
end

module PosMap = Map.Make (Pos)
module PosSet = Set.Make (Pos)

let pipe_opt x y c =
  match c with
  | '|' -> Some ((x, y), [ x, y + 1; x, y - 1 ])
  | '-' -> Some ((x, y), [ x + 1, y; x - 1, y ])
  | 'L' -> Some ((x, y), [ x, y - 1; x + 1, y ])
  | 'J' -> Some ((x, y), [ x, y - 1; x - 1, y ])
  | '7' -> Some ((x, y), [ x, y + 1; x - 1, y ])
  | 'F' -> Some ((x, y), [ x, y + 1; x + 1, y ])
  | 'S' -> Some ((x, y), [ x + 1, y; x, y + 1; x - 1, y; x, y - 1 ])
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

let first_pos =
  PosMap.find start pipe_map
  |> List.find (fun x ->
    match PosMap.find_opt x pipe_map with
    | Some pos_list -> List.exists (( = ) start) pos_list
    | None -> false)
;;

let rec walk step acc curr =
  let next = PosMap.find curr pipe_map |> List.find (fun x -> x <> List.hd acc) in
  if next = start
  then (step / 2) + 1, PosSet.of_list (curr :: acc), curr
  else walk (step + 1) (curr :: acc) next
;;

let part1, path, last_pos = walk 0 [ start ] first_pos

let start_shape =
  let a = Pos.sub start first_pos in
  let b = Pos.sub start last_pos in
  match Pos.add a b with
  | 0, 0 -> if fst a = fst b then '|' else '-'
  | 1, 1 -> 'F'
  | 1, -1 -> 'L'
  | -1, 1 -> '7'
  | -1, -1 -> 'J'
  | _ -> failwith "invalid shape"
;;

let print_pipe c =
  print_string
  @@
  match c with
  | '|' -> "│"
  | '-' -> "─"
  | 'L' -> "╰"
  | 'J' -> "╯"
  | '7' -> "╮"
  | 'F' -> "╭"
  | c -> Char.escaped c
;;

let calc_part2 () =
  let enclosed = ref 0 in
  let last = ref ' ' in
  List.iteri
    (fun y line ->
      let count = ref 0 in
      List.iteri (fun x c ->
        if PosSet.mem (x, y) path
        then (
          let p = if c = 'S' then start_shape else c in
          if p = '|' || (p = 'J' && !last = 'F') || (p = '7' && !last = 'L')
          then count := !count + 1;
          if p = 'F' || p = 'L' then last := p;
          print_pipe c)
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
  let part2 = calc_part2 () in
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int part2
;;
