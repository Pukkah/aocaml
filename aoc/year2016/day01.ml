let input =
  "R2, L5, L4, L5, R4, R1, L4, R5, R3, R1, L1, L1, R4, L4, L1, R4, L4, R4, L3, R5, R4, \
   R1, R3, L1, L1, R1, L2, R5, L4, L3, R1, L2, L2, R192, L3, R5, R48, R5, L2, R76, R4, \
   R2, R1, L1, L5, L1, R185, L5, L1, R5, L4, R1, R3, L4, L3, R1, L5, R4, L4, R4, R5, L3, \
   L1, L2, L4, L3, L4, R2, R2, L3, L5, R2, R5, L1, R1, L3, L5, L3, R4, L4, R3, L1, R5, \
   L3, R2, R4, R2, L1, R3, L1, L3, L5, R4, R5, R2, R2, L5, L3, L1, L1, L5, L2, L3, R3, \
   R3, L3, L4, L5, R2, L1, R1, R3, R4, L2, R1, L1, R3, R3, L4, L2, R5, R5, L1, R4, L5, \
   L5, R1, L5, R4, R2, L1, L4, R1, L1, L1, L5, R3, R4, L2, R1, R2, R1, R1, R3, L5, R1, \
   R4"
;;

type direction =
  | Left
  | Right

let parse_instructions input =
  input
  |> String.split_on_char ','
  |> List.map (fun s ->
    let s' = String.trim s in
    ( (match s'.[0] with
       | 'L' -> Left
       | 'R' -> Right
       | _ -> failwith "Invalid direction")
    , int_of_string (String.sub s' 1 (String.length s' - 1)) ))
;;

let get_path instructions =
  let rec walk (x, y) (dx, dy) path = function
    | 0 -> path
    | n ->
      let x', y' = x + dx, y + dy in
      walk (x', y') (dx, dy) ((x', y') :: path) (n - 1)
  in
  let turn (dx, dy) = function
    | Left -> -dy, dx
    | Right -> dy, -dx
  in
  let rec go (x, y) (dx, dy) path = function
    | [] -> path
    | (direction, distance) :: instructions ->
      let dx', dy' = turn (dx, dy) direction in
      let path' = walk (x, y) (dx', dy') path distance in
      go (List.hd path') (dx', dy') path' instructions
  in
  go (0, 0) (0, 1) [ 0, 0 ] instructions
;;

let distance_of_point (x, y) = abs x + abs y

module PointSet = Set.Make (struct
    type t = int * int

    let compare = compare
  end)

let get_first_crossing path =
  let rec go set = function
    | [] -> None
    | point :: path' ->
      if PointSet.mem point set then Some point else go (PointSet.add point set) path'
  in
  go PointSet.empty (List.rev path)
;;

let run () =
  input |> parse_instructions |> get_path |> List.hd |> distance_of_point |> print_int;
  print_newline ();
  input
  |> parse_instructions
  |> get_path
  |> get_first_crossing
  |> Option.get
  |> distance_of_point
  |> print_int
;;
