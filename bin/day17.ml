let input =
  "2413432311323\n\
   3215453535623\n\
   3255245654254\n\
   3446585845452\n\
   4546657867536\n\
   1438598798454\n\
   4457876987766\n\
   3637877979653\n\
   4654967986887\n\
   4564679986453\n\
   1224686865563\n\
   2546548887735\n\
   4322674655533"
;;

let parse_input input =
  let lines = Util.get_lines input in
  let width = String.length (List.hd lines) in
  let height = List.length lines in
  let grid = Array.make_matrix height width 0 in
  List.iteri
    (fun i line -> String.iteri (fun j c -> grid.(i).(j) <- int_of_char c) line)
    lines;
  width, height, grid
;;

module PriorityQueue = struct
  type 'a t = ('a * int) list ref

  let create () = ref []
  let is_empty pq = !pq = []

  let add (x, p) pq =
    let rec loop = function
      | [] -> [ x, p ]
      | (y, q) :: ys as l -> if p <= q then (x, p) :: l else (y, q) :: loop ys
    in
    pq := loop !pq
  ;;

  let take pq =
    match !pq with
    | [] -> failwith "PriorityQueue.take"
    | (x, p) :: xs ->
      pq := xs;
      x, p
  ;;
end

let in_bounds (x, y) grid =
  let height = Array.length grid in
  let width = Array.length grid.(0) in
  x >= 0 && y >= 0 && x < width && y < height
;;

let neighbors (x, y) grid =
  let steps = [ -1, 0; 1, 0; 0, -1; 0, 1 ] in
  List.fold_left
    (fun acc (dx, dy) ->
      List.fold_left
        (fun acc i ->
          let nx, ny = x + (dx * i), y + (dy * i) in
          if in_bounds (nx, ny) grid then (nx, ny) :: acc else acc)
        acc
        [ 1; 2; 3 ])
    []
    steps
;;

let dijkstra grid start goal =
  let dist = Hashtbl.create (Array.length grid * Array.length grid.(0)) in
  let pq = PriorityQueue.create () in
  Hashtbl.add dist start 0;
  PriorityQueue.add (0, start) pq;
  (try
     while not (PriorityQueue.is_empty pq) do
       let _, u = PriorityQueue.take pq in
       if u = goal then raise Exit;
       List.iter
         (fun v ->
           let alt = Hashtbl.find dist u + grid.(fst v).(snd v) in
           if (not (Hashtbl.mem dist v)) || alt < Hashtbl.find dist v
           then (
             Hashtbl.replace dist v alt;
             PriorityQueue.add (alt, v) pq))
         (neighbors u grid)
     done
   with
   | Exit -> ());
  Hashtbl.find dist goal
;;

let run () =
  let width, height, grid = parse_input input in
  let start = 0, 0 in
  let goal = width - 1, height - 1 in
  Printf.printf "Shortest distance: %d\n" (dijkstra grid start goal)
;;
