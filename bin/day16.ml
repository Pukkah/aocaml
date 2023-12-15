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

let run () =
  let grid = parse_input input in
  let ans = solve (0, 0) (1, 0) grid in
  print_int ans
;;
