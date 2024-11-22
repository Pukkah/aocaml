let input' = "flqrgnkx"
let input = "hfdlxzhv"

let hex_to_bin = function
  | '0' -> "0000"
  | '1' -> "0001"
  | '2' -> "0010"
  | '3' -> "0011"
  | '4' -> "0100"
  | '5' -> "0101"
  | '6' -> "0110"
  | '7' -> "0111"
  | '8' -> "1000"
  | '9' -> "1001"
  | 'A' | 'a' -> "1010"
  | 'B' | 'b' -> "1011"
  | 'C' | 'c' -> "1100"
  | 'D' | 'd' -> "1101"
  | 'E' | 'e' -> "1110"
  | 'F' | 'f' -> "1111"
  | _ -> failwith "invalid hex character"
;;

let bin_str_of_hex_str = Base.String.concat_map ~f:hex_to_bin ?sep:None
let knot_hash = Day10.knot_hash_of_string

let part1 input =
  List.init 128 (fun i ->
    input ^ "-" ^ string_of_int i
    |> knot_hash
    |> bin_str_of_hex_str
    |> Base.String.count ~f:(( = ) '1'))
  |> List.fold_left ( + ) 0
;;

let part2 input =
  let count = ref 0
  and _visited = Array.make_matrix 128 128 false
  and _matrix =
    Array.init 128 (fun i ->
      input ^ "-" ^ string_of_int i
      |> knot_hash
      |> bin_str_of_hex_str
      |> Base.String.to_array
      |> Array.map (( = ) '1'))
  and directions (x, y) = [ x + 1, y; x, y + 1; x - 1, y; x, y - 1 ] in
  let rec dfs (row, col) =
    _visited.(row).(col) <- true;
    directions (row, col)
    |> List.iter (fun (x, y) ->
      if x >= 0 && x < 128 && y >= 0 && y < 128 && _matrix.(x).(y) && not _visited.(x).(y)
      then dfs (x, y))
  in
  for row = 0 to 127 do
    for col = 0 to 127 do
      if _matrix.(row).(col) && not _visited.(row).(col)
      then (
        dfs (row, col);
        incr count)
    done
  done;
  !count
;;

let run () =
  part1 input |> print_int |> print_newline;
  part2 input |> print_int |> print_newline
;;
