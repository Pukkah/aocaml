let test = "125 17"

let solve input blinks =
  let stones = Util.parse_numbers input
  and memory = Hashtbl.create 100000 in
  let rec blink stone blinks =
    if blinks = 0
    then 1
    else if Hashtbl.mem memory (stone, blinks)
    then Hashtbl.find memory (stone, blinks)
    else (
      let res =
        if stone = 0
        then blink 1 (blinks - 1)
        else (
          let str_stone = string_of_int stone in
          let len = String.length str_stone in
          if len mod 2 = 0
          then (
            let mid = len / 2 in
            let left = int_of_string (String.sub str_stone 0 mid) in
            let right = int_of_string (String.sub str_stone mid (len - mid)) in
            blink left (blinks - 1) + blink right (blinks - 1))
          else blink (stone * 2024) (blinks - 1))
      in
      Hashtbl.add memory (stone, blinks) res;
      res)
  in
  List.fold_left (fun acc stone -> acc + blink stone blinks) 0 stones
;;

let run ?(input = test) () =
  solve input 25 |> print_int |> print_newline;
  solve input 75 |> print_int |> print_newline
;;
