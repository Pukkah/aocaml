let coordinates n =
  if n = 1
  then 0, 0
  else (
    let t = sqrt (float_of_int n) in
    let k = int_of_float (ceil ((t -. 1.0) /. 2.0)) in
    let s = (2 * k) + 1 in
    let max_n = s * s in
    let steps = max_n - n in
    let side_length = 2 * k in
    let side = steps / side_length in
    let offset = steps mod side_length in
    let x, y =
      if side = 0
      then k - offset, -k
      else if side = 1
      then -k, -k + offset
      else if side = 2
      then -k + offset, k
      else k, k - offset
    in
    x, y)
;;

let run () =
  let x, y = coordinates 312051 in
  abs x + abs y |> print_int;
  print_newline ()
;;
