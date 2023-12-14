let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

let hash str =
  Seq.fold_left (fun acc c -> (acc + Char.code c) * 17 mod 256) 0 @@ String.to_seq str
;;

let part1 input =
  input |> String.split_on_char ',' |> List.map hash |> List.fold_left ( + ) 0
;;

let run () = Printf.printf "Day 14: %d\n" (part1 input)
