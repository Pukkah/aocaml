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

let test = "2333133121414131402"
let%test _ = part1 test = 1928

let run ?(input = test) () = part1 input |> print_int |> print_newline
