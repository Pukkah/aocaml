let input =
  "???.### 1,1,3\n\
   .??..??...?##. 1,1,3\n\
   ?#?#?#?#?#?#?#? 1,3,1,6\n\
   ????.#...#... 4,1,1\n\
   ????.######..#####. 1,6,5\n\
   ?###???????? 3,2,1"
;;

let parse line =
  match String.split_on_char ' ' line with
  | [ springs; groups ] ->
    ( springs |> String.to_seq |> List.of_seq
    , groups |> String.split_on_char ',' |> List.map int_of_string )
  | _ -> failwith "invalid input"
;;

let lines = Util.get_lines input |> List.map parse

module Permutation = struct
  type t = char list * int list * int * bool

  let compare (cl1, il1, n1, b1) (cl2, il2, n2, b2) =
    match compare n1 n2 with
    | 0 ->
      (match compare b1 b2 with
       | 0 ->
         (match compare il1 il2 with
          | 0 -> compare cl1 cl2
          | x -> x)
       | x -> x)
    | x -> x
  ;;
end

module PermMap = Map.Make (Permutation)

let permutations (springs, groups) =
  let cache = ref PermMap.empty in
  let rec permute rem rem_groups group gap =
    let key = rem, rem_groups, group, gap in
    if PermMap.mem key !cache
    then PermMap.find key !cache
    else (
      let result =
        match rem, rem_groups, group, gap with
        | [], [], 0, _ -> 1
        (* unknown *)
        | '?' :: t, gh :: gt, 0, false ->
          permute t gt (gh - 1) (gh = 1) + permute t rem_groups 0 false
        (* not damaged *)
        | '?' :: t, [], 0, false | '?' :: t, _, 0, true | '.' :: t, _, 0, _ ->
          permute t rem_groups 0 false
        (* damaged *)
        | '#' :: t, gh :: gt, 0, false -> permute t gt (gh - 1) (gh = 1)
        | '?' :: t, _, group, false | '#' :: t, _, group, false ->
          permute t rem_groups (group - 1) (group = 1)
        (* impossible *)
        | _ -> 0
      in
      cache := PermMap.add key result !cache;
      result)
  in
  permute springs groups 0 false
;;

let unfold_line n line =
  let rec unfold acc = function
    | 1 -> acc
    | n -> unfold (fst line @ ('?' :: fst acc), snd line @ snd acc) (n - 1)
  in
  unfold line n
;;

let part1 = lines |> List.map permutations |> Util.sum
let part2 = lines |> List.map (unfold_line 5) |> List.map permutations |> Util.sum

let run () =
  print_endline @@ "Part1: " ^ string_of_int part1;
  print_endline @@ "Part2: " ^ string_of_int part2
;;
