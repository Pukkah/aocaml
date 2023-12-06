let input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"
let lines = Util.get_lines input

let get_value str =
  let rec aux = function
    | ('0' .. '9' as c) :: _ -> Char.escaped c
    | _ :: tl -> aux tl
    | [] -> raise Not_found
  in
  let list = String.to_seq str |> List.of_seq in
  int_of_string @@ aux list ^ aux (List.rev list)
;;

let part1 = List.map get_value lines |> List.fold_left ( + ) 0
let run () = print_endline @@ "Part 1: " ^ string_of_int part1
