let input' = "0\t2\t7\t0"
let input = "5\t1\t10\t0\t1\t7\t13\t14\t3\t12\t8\t10\t7\t12\t0\t6"
let parse input = String.split_on_char '\t' input |> List.map int_of_string
let hash memory = Array.map string_of_int memory |> Base.String.concat_array ~sep:"-"

let debug seed =
  let memory = Array.of_list seed
  and seen = Hashtbl.create 1000
  and cycle = ref 0 in
  let next bank = if bank + 1 = Array.length memory then 0 else bank + 1 in
  let rec redistribute bank = function
    | 0 -> ()
    | n ->
      let next' = next bank in
      memory.(next') <- memory.(next') + 1;
      redistribute next' (n - 1)
  in
  while hash memory |> Hashtbl.mem seen |> not do
    Hashtbl.replace seen (hash memory) !cycle;
    let bank, blocks =
      Array.to_seqi memory
      |> List.of_seq
      |> List.sort (fun (_, a) (_, b) -> b - a)
      |> List.hd
    in
    memory.(bank) <- 0;
    redistribute bank blocks;
    incr cycle
  done;
  Hashtbl.find seen (hash memory), !cycle
;;

let run () =
  let first, second = parse input |> debug in
  second |> print_int |> print_newline;
  second - first |> print_int |> print_newline
;;
