let input = "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."
let lines = Util.get_lines input

module PipeMap = Map.Make (struct
    type t = int * int

    let compare = compare
  end)

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
  List.fold_left (fun acc (pos, pipe) -> PipeMap.add pos pipe acc) PipeMap.empty pipes
;;

let start = List.find (fun x -> List.length (snd x) = 4) pipes |> fst
let fist_pos = PipeMap.find start pipe_map |> List.find (fun x -> PipeMap.mem x pipe_map)

let rec walk steps prev curr =
  let next = PipeMap.find curr pipe_map |> List.find (fun x -> x <> prev) in
  if next = start then (steps / 2) + 1 else walk (steps + 1) curr next
;;

let part1 = walk 0 start fist_pos
let run () = print_int part1
