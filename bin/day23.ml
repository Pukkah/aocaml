let input =
  "#.#####################\n\
   #.......#########...###\n\
   #######.#########.#.###\n\
   ###.....#.>.>.###.#.###\n\
   ###v#####.#v#.###.#.###\n\
   ###.>...#.#.#.....#...#\n\
   ###v###.#.#.#########.#\n\
   ###...#.#.#.......#...#\n\
   #####.#.#.#######.#.###\n\
   #.....#.#.#.......#...#\n\
   #.#####.#.#.#########v#\n\
   #.#...#...#...###...>.#\n\
   #.#.#v#######v###.###v#\n\
   #...#.>.#...>.>.#.###.#\n\
   #####v#.#.###v#.#.###.#\n\
   #.....#...#...#.#.#...#\n\
   #.#########.###.#.#.###\n\
   #...###...#...#...#.###\n\
   ###.###.#.###v#####v###\n\
   #...#...#.#.>.>.#.>.###\n\
   #.###.###.#.###.#.#v###\n\
   #.....###...###...#...#\n\
   #####################.#"
;;

module MoveSet = Set.Make (struct
    type t = int * int * int

    let compare = compare
  end)

let parse_input input =
  let lines = Util.get_lines input in
  let width = String.length (List.hd lines)
  and height = List.length lines in
  let grid = Array.make_matrix width height ' ' in
  List.iteri (fun y line -> String.iteri (fun x c -> grid.(x).(y) <- c) line) lines;
  grid
;;

let get_start_and_finish grid =
  let start_x =
    Array.to_seqi grid.(0) |> List.of_seq |> List.find (fun (_, c) -> c = '.') |> fst
  and finish_x =
    Array.to_seqi grid.(Array.length grid - 1)
    |> List.of_seq
    |> List.find (fun (_, c) -> c = '.')
    |> fst
  in
  (start_x, 0), (finish_x, Array.length grid - 1)
;;

let run () = ()
