let test =
  "....#.....\n\
   .........#\n\
   ..........\n\
   ..#.......\n\
   .......#..\n\
   ..........\n\
   .#..^.....\n\
   ........#.\n\
   #.........\n\
   ......#..."
;;

type state =
  { map : char array array
  ; lx : int
  ; ly : int
  ; init_pos : int * int
  }

let parse input =
  let map =
    Base.String.split_lines input |> List.map Base.String.to_array |> Array.of_list
  in
  let lx, ly = Array.length map, Array.length map.(0)
  and init_pos =
    Array.find_mapi
      (fun x row ->
        Array.find_mapi (fun y c -> if c = '^' then Some (x, y) else None) row)
      map
    |> Option.get
  in
  { map; lx; ly; init_pos }
;;

module SSet = Set.Make (String)

let make_pair = Printf.sprintf "%d|%d"

let patrol { map; lx; ly; init_pos } =
  let check_pos x y =
    if x < 0 || x >= lx || y < 0 || y >= ly then None else Some map.(x).(y)
  in
  let rec walk path (x, y) (dx, dy) =
    let path = SSet.add (make_pair x y) path in
    match check_pos (x + dx) (y + dy) with
    | None -> path
    | Some '#' ->
      let dx, dy = dy, -dx in
      walk path (x, y) (dx, dy)
    | Some _ -> walk path (x + dx, y + dy) (dx, dy)
  in
  walk SSet.empty init_pos (-1, 0)
;;

let part1 input = parse input |> patrol |> SSet.cardinal
let make_quad x y dx dy = Printf.sprintf "%d|%d|%d|%d" x y dx dy

let has_loop state pos =
  let block_pos = Scanf.sscanf pos "%d|%d" (fun x y -> x, y) in
  let check_pos x y =
    if x < 0 || x >= state.lx || y < 0 || y >= state.ly
    then None
    else Some (if (x, y) = block_pos then '#' else state.map.(x).(y))
  in
  let rec walk loop (x, y) (dx, dy) =
    match check_pos (x + dx) (y + dy) with
    | None -> false
    | Some '#' ->
      if SSet.mem (make_quad x y dx dy) loop
      then true
      else (
        let loop = SSet.add (make_quad x y dx dy) loop
        and dx, dy = dy, -dx in
        walk loop (x, y) (dx, dy))
    | Some _ -> walk loop (x + dx, y + dy) (dx, dy)
  in
  if block_pos = state.init_pos then false else walk SSet.empty state.init_pos (-1, 0)
;;

let part2 input =
  let state = parse input in
  patrol state |> SSet.elements |> List.filter (has_loop state) |> List.length
;;

let run ?(input = test) () =
  part1 input |> print_int |> print_newline;
  part2 input |> print_int |> print_newline
;;
