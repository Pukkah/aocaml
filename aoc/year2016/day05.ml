let input = "abc"
let hash s i = s ^ string_of_int i |> Digest.string |> Digest.to_hex

let decode input =
  let rec aux acc found i =
    if found = 8
    then acc
    else (
      let h = hash input i in
      if String.sub h 0 5 = "00000"
      then aux (h.[5] :: acc) (found + 1) (i + 1)
      else aux acc found (i + 1))
  in
  aux [] 0 0 |> List.rev |> List.to_seq |> String.of_seq
;;

let decode2 input =
  let psw = Array.make 8 '#'
  and i = ref (-1)
  and found = ref 0 in
  while !found < 8 do
    incr i;
    let h = hash input !i in
    if String.sub h 0 5 = "00000"
    then (
      let pos =
        match int_of_string_opt (Char.escaped h.[5]) with
        | Some a -> a
        | None -> 8
      in
      if pos < 8 && psw.(pos) = '#'
      then (
        psw.(pos) <- h.[6];
        incr found))
  done;
  String.of_seq @@ Array.to_seq psw
;;

let run () =
  decode input |> print_endline;
  decode2 input |> print_endline
;;
