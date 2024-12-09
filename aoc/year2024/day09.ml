let parse input =
  let arr = Dynarray.create () in
  String.iteri
    (fun i c ->
      for _ = 1 to int_of_char c - 48 do
        Dynarray.add_last arr (if i mod 2 = 0 then i / 2 else -1)
      done)
    input;
  arr
;;

let part1 input =
  let arr = parse input
  and sum = ref 0
  and i = ref 0 in
  while !i < Dynarray.length arr do
    let value = ref (Dynarray.get arr !i) in
    while !value = -1 do
      value := Dynarray.pop_last arr
    done;
    sum := !sum + (!i * !value);
    incr i
  done;
  !sum
;;

type block =
  { idx : int
  ; len : int
  ; value : int
  }

let parse2 input =
  let i = ref 0
  and blocks = ref [] in
  String.iteri
    (fun j c ->
      let len = int_of_char c - 48 in
      if len <> 0
      then (
        blocks
        := { idx = !i; len; value = (if j mod 2 = 0 then j / 2 else -1) } :: !blocks;
        i := !i + len))
    input;
  List.rev !blocks
;;

let part2 input =
  let blocks = parse2 input in
  let rec find_gap_and_place acc rest item =
    match rest with
    | hd :: tl ->
      if hd.idx = item.idx
      then List.rev_append acc rest
      else if hd.value = -1 && hd.len >= item.len
      then (
        let new_acc = { item with idx = hd.idx } :: acc
        and new_tl = List.filter (fun x -> x.idx <> item.idx) tl in
        if hd.len > item.len
        then
          List.rev_append
            new_acc
            ({ idx = hd.idx + item.len; len = hd.len - item.len; value = -1 } :: new_tl)
        else List.rev_append new_acc new_tl)
      else find_gap_and_place (hd :: acc) tl item
    | _ -> failwith "should not happen"
  in
  List.fold_right
    (fun item acc -> if item.value <> -1 then find_gap_and_place [] acc item else acc)
    blocks
    blocks
  |> List.fold_left
       (fun acc item ->
         if item.value = -1
         then acc
         else
           Seq.ints item.idx
           |> Seq.take item.len
           |> Seq.fold_left (fun acc m -> acc + (m * item.value)) acc)
       0
;;

let test = "2333133121414131402"
let%test _ = part1 test = 1928
let%test _ = part2 test = 2858

let run ?(input = test) () =
  part1 input |> print_int |> print_newline;
  part2 input |> print_int |> print_newline
;;
