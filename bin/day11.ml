module Pos = struct
  type t = int * int

  let dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)
  let compare = compare
end

module PosSet = Set.Make (Pos)
module IntSet = Set.Make (Int)

let input =
  "...#......\n\
   .......#..\n\
   #.........\n\
   ..........\n\
   ......#...\n\
   .#........\n\
   .........#\n\
   ..........\n\
   .......#..\n\
   #...#....."
;;

let galaxies, empty_x, empyt_y =
  let lines = Util.get_lines input |> List.mapi (fun y l -> y, l) in
  let galaxies, occupied_x, empty_y =
    lines
    |> List.fold_left
         (fun (g, ox, ey) (y, line) ->
           let res =
             String.to_seqi line
             |> Seq.fold_left
                  (fun acc (x, c) -> if c = '#' then PosSet.add (x, y) acc else acc)
                  PosSet.empty
           in
           if PosSet.is_empty res
           then g, ox, IntSet.add y ey
           else
             ( PosSet.union g res
             , IntSet.union
                 ox
                 (PosSet.to_seq res |> Seq.map (fun (x, _) -> x) |> IntSet.of_seq)
             , ey ))
         (* galaxies, occupied_x, empyt_y *)
         (PosSet.empty, IntSet.empty, IntSet.empty)
  in
  let empty_x =
    let exes =
      Array.init (lines |> List.hd |> snd |> String.length) (fun i -> i)
      |> Array.to_seq
      |> IntSet.of_seq
    in
    IntSet.diff exes occupied_x
  in
  galaxies, empty_x, empty_y
;;

let count_range low high set =
  let _, _, lower_set = IntSet.split low set in
  let upper_set, _, _ = IntSet.split high lower_set in
  IntSet.cardinal upper_set
;;

let get_distance (x1, y1) (x2, y2) mult =
  Pos.dist (x1, y1) (x2, y2)
  + (count_range (min x1 x2) (max x1 x2) empty_x * (mult - 1))
  + (count_range (min y1 y2) (max y1 y2) empyt_y * (mult - 1))
;;

let solve mult =
  PosSet.fold
    (fun a acc ->
      let _, _, to_check = PosSet.split a galaxies in
      acc + PosSet.fold (fun b iacc -> iacc + get_distance a b mult) to_check 0)
    galaxies
    0
;;

let part1 = solve 2
let part2 = solve 1000000

let run () =
  print_endline @@ "Part 1: " ^ string_of_int part1;
  print_endline @@ "Part 2: " ^ string_of_int part2
;;
