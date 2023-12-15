let input =
  {|
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
|}
  |> String.trim
;;

let parse_input input =
  let lines = Util.get_lines input in
  let width = String.length (List.hd lines)
  and height = List.length lines in
  let grid = Array.make_matrix height width '.' in
  List.iteri (fun y line -> String.iteri (fun x c -> grid.(y).(x) <- c) line) lines;
  grid
;;

module MoveSet = Set.Make (struct
    type t = int * int * int * int

    let compare = compare
  end)

module PosSet = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

let solve start dir grid =
  let seen = ref MoveSet.empty in
  let rec walk (x, y) (vx, vy) =
    if (not @@ MoveSet.mem (x, y, vx, vy) !seen)
       && y >= 0
       && y < Array.length grid
       && x >= 0
       && x < Array.length grid.(0)
    then (
      seen := MoveSet.add (x, y, vx, vy) !seen;
      match grid.(y).(x), (vx, vy) with
      | '.', _ | '-', (_, 0) | '|', (0, _) -> walk (x + vx, y + vy) (vx, vy)
      | '|', _ ->
        walk (x, y + 1) (0, 1);
        walk (x, y - 1) (0, -1)
      | '-', _ ->
        walk (x + 1, y) (1, 0);
        walk (x - 1, y) (-1, 0)
      | '/', _ -> walk (x - vy, y - vx) (-vy, -vx)
      | '\\', _ -> walk (x + vy, y + vx) (vy, vx)
      | _ -> ())
  in
  walk start dir;
  let ilum =
    MoveSet.to_seq !seen |> Seq.map (fun (x, y, _, _) -> x, y) |> PosSet.of_seq
  in
  PosSet.cardinal ilum
;;

let part1 grid = solve (0, 0) (1, 0) grid

let part2 grid =
  let best = ref 0
  and max_x = Array.length grid.(0) - 1
  and max_y = Array.length grid - 1 in
  for x = 0 to max_x do
    best := max !best @@ solve (x, 0) (0, 1) grid
  done;
  for x = 0 to max_x do
    best := max !best @@ solve (x, max_y) (0, -1) grid
  done;
  for y = 0 to max_y do
    best := max !best @@ solve (0, y) (1, 0) grid
  done;
  for y = 0 to max_y do
    best := max !best @@ solve (max_x, y) (-1, 0) grid
  done;
  !best
;;

let run () =
  let grid = parse_input input in
  Printf.printf "Part 1: %d\n" @@ part1 grid;
  Printf.printf "Part 2: %d\n" @@ part2 grid
;;
