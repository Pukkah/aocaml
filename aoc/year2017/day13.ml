let input' = "0: 3\n1: 2\n4: 4\n6: 4"

let input =
  "0: 3\n\
   1: 2\n\
   2: 4\n\
   4: 4\n\
   6: 5\n\
   8: 6\n\
   10: 6\n\
   12: 6\n\
   14: 6\n\
   16: 8\n\
   18: 8\n\
   20: 8\n\
   22: 8\n\
   24: 10\n\
   26: 8\n\
   28: 8\n\
   30: 12\n\
   32: 14\n\
   34: 12\n\
   36: 10\n\
   38: 12\n\
   40: 12\n\
   42: 9\n\
   44: 12\n\
   46: 12\n\
   48: 12\n\
   50: 12\n\
   52: 14\n\
   54: 14\n\
   56: 14\n\
   58: 12\n\
   60: 14\n\
   62: 14\n\
   64: 12\n\
   66: 14\n\
   70: 14\n\
   72: 14\n\
   74: 14\n\
   76: 14\n\
   80: 18\n\
   88: 20\n\
   90: 14\n\
   98: 17"
;;

let parse input =
  Base.String.split_lines input
  |> List.map (fun s ->
    let i, d = Base.String.lsplit2_exn ~on:':' s in
    int_of_string i, String.trim d |> int_of_string)
;;

let severity acc (i, d) = if i mod (2 * (d - 1)) = 0 then acc + (i * d) else acc

let delayed firewall =
  let delay = ref 0
  and safe = ref false in
  while not !safe do
    incr delay;
    if List.for_all (fun (i, d) -> (!delay + i) mod (2 * (d - 1)) != 0) firewall
    then safe := true
  done;
  !delay
;;

let run () =
  parse input |> List.fold_left severity 0 |> print_int |> print_newline;
  parse input |> delayed |> print_int |> print_newline
;;
