let chechsum n = n mod 65536

let part1 a b =
  let next v fac = v * fac mod 2147483647 in
  let rec aux acc a b = function
    | 0 -> acc
    | n ->
      let a = next a 16807
      and b = next b 48271 in
      if chechsum a = chechsum b then aux (acc + 1) a b (n - 1) else aux acc a b (n - 1)
  in
  aux 0 a b 40_000_000
;;

let part2 a b =
  let rec next v fac m =
    let n = v * fac mod 2147483647 in
    if n mod m = 0 then n else next n fac m
  in
  let rec aux acc a b = function
    | 0 -> acc
    | n ->
      let a = next a 16807 4
      and b = next b 48271 8 in
      if chechsum a = chechsum b then aux (acc + 1) a b (n - 1) else aux acc a b (n - 1)
  in
  aux 0 a b 5_000_000
;;

let run () =
  part1 65 8921 |> print_int |> print_newline;
  part2 65 8921 |> print_int |> print_newline
;;
