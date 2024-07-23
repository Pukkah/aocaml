let input =
  "467..114..\n\
   ...*......\n\
   ..35..633.\n\
   ......#...\n\
   617*......\n\
   .....+.58.\n\
   ..592.....\n\
   ......755.\n\
   ...$.*....\n\
   .664.598.."
;;

let lines = Util.get_lines input
let max_x = String.length (List.hd lines)
let max_y = List.length lines

type part =
  { x1 : int
  ; y1 : int
  ; x2 : int
  ; y2 : int
  ; num : int
  }

type symbol =
  { x : int
  ; y : int
  ; c : char
  }

type entry =
  | Part of part
  | Symbol of symbol

let parse_line y line =
  let y1 = if y > 0 then y - 1 else 0 in
  let y2 = if y < max_y then y + 1 else max_y in
  let save tmp =
    let num =
      List.fold_left (fun acc (_, c) -> Char.escaped c ^ acc) "" tmp |> int_of_string
    in
    let x1 = List.hd tmp |> fst in
    let x2 = x1 + List.length tmp in
    Part { x1; y1; x2; y2; num }
  in
  let rec aux tmp acc = function
    | [] -> if tmp <> [] then save tmp :: acc else acc
    | (_, '.') :: tl -> if tmp <> [] then aux [] (save tmp :: acc) tl else aux [] acc tl
    | ((_, '0' .. '9') as hd) :: tl -> aux (hd :: tmp) acc tl
    | hd :: tl ->
      let symb = Symbol { x = fst hd; y; c = snd hd } in
      if tmp <> [] then aux [] (save tmp :: symb :: acc) tl else aux [] (symb :: acc) tl
  in
  aux [] [] (String.to_seqi line |> List.of_seq)
;;

let entries = List.flatten @@ List.mapi parse_line lines

let run () =
  List.iter
    (fun e ->
      print_newline ();
      match e with
      | Symbol symb -> print_char symb.c
      | Part p -> print_int p.num)
    entries
;;
