let input =
  "...........\n\
   .....###.#.\n\
   .###.##..#.\n\
   ..#.#...#..\n\
   ....#.#....\n\
   .##..S####.\n\
   .##..#...#.\n\
   .......##..\n\
   .##.#.####.\n\
   .##..##.##.\n\
   ..........."
;;

let steps = 6 (* 6 for example, 64 for real input *)

module VecSet = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

module MoveSet = Set.Make (struct
    type t = int * int * int

    let compare = compare
  end)

let parse_input input =
  let lines = Util.get_lines input
  and start = ref (0, 0) in
  Array.make_matrix (String.length @@ List.hd lines) (List.length lines) '.'
  |> fun grid ->
  List.iteri
    (fun y line ->
      String.iteri
        (fun x c ->
          grid.(x).(y) <- c;
          if c = 'S' then start := x, y)
        line)
    lines;
  !start, grid
;;

let part1 grid start steps =
  let reachable = ref VecSet.empty
  and visited = ref MoveSet.empty
  and max_x = Array.length grid.(0) - 1
  and max_y = Array.length grid - 1 in
  let rec aux (x, y) steps =
    if (not (MoveSet.mem (x, y, steps) !visited))
       && steps >= 0
       && x >= 0
       && x <= max_x
       && y >= 0
       && y <= max_y
       && grid.(x).(y) <> '#'
    then
      if steps = 0
      then reachable := VecSet.add (x, y) !reachable
      else (
        visited := MoveSet.add (x, y, steps) !visited;
        let steps = steps - 1 in
        aux (x + 1, y) steps;
        aux (x - 1, y) steps;
        aux (x, y + 1) steps;
        aux (x, y - 1) steps)
  in
  aux start steps;
  VecSet.cardinal !reachable
;;

let run () =
  let start, grid = parse_input input in
  Printf.printf "Part 1: %d" (part1 grid start steps)
;;
