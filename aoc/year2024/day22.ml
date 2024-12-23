let parse input = Base.String.split_lines input |> List.map int_of_string
let mix sv v = v lxor sv
let prune v = v mod 16777216

let next secret =
  let secret = secret * 64 |> mix secret |> prune in
  let secret = secret / 32 |> mix secret |> prune in
  let secret = secret * 2048 |> mix secret |> prune in
  secret
;;

let rec calculate_nth_value ~f ~n current =
  if n = 0 then current else calculate_nth_value ~f (f current) ~n:(n - 1)
;;

let part1 input =
  let seeds = parse input in
  Base.List.sum (module Base.Int) ~f:(calculate_nth_value ~f:next ~n:2000) seeds
;;

module SMap = Map.Make (String)

let make_key a b c d = Printf.sprintf "%d,%d,%d,%d" a b c d
let ones_digit v = v mod 10

let merge_maps maps =
  List.fold_left
    (SMap.merge (fun _ value1 value2 ->
       match value1, value2 with
       | Some v, None | None, Some v -> Some v
       | Some v1, Some v2 -> Some (v1 + v2)
       | _ -> None))
    SMap.empty
    maps
;;

let part2 input =
  let seeds = parse input in
  let aux seed =
    let seq_map = ref SMap.empty in
    let rec loop v n seq =
      let nv = next v in
      let bananas = ones_digit nv in
      let diff = bananas - ones_digit v in
      let seq = diff :: seq in
      (match seq with
       | a :: b :: c :: d :: _ ->
         let key = make_key a b c d in
         if not @@ SMap.mem key !seq_map then seq_map := SMap.add key bananas !seq_map
       | _ -> ());
      if n > 1 then loop nv (n - 1) seq
    in
    loop seed 2000 [];
    !seq_map
  in
  List.map aux seeds
  |> merge_maps
  |> SMap.bindings
  |> List.sort (fun (_, v1) (_, v2) -> compare v2 v1)
  |> List.hd
  |> snd
;;

let test1 = "1\n10\n100\n2024"
let%test _ = part1 test1 = 37327623
let test2 = "1\n2\n3\n2024"
let%test _ = part2 test2 = 23

let run ?input () =
  let input1, input2 =
    match input with
    | Some s -> s, s
    | None -> test1, test2
  in
  part1 input1 |> print_int |> print_newline;
  part2 input2 |> print_int |> print_newline
;;
