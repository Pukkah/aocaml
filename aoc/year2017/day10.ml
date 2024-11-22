let input' = "3,4,1,5"
let input = "206,63,255,131,65,80,238,157,254,24,133,2,16,0,1,3"
let parse input = String.split_on_char ',' input |> List.map int_of_string

let ascii_parse input =
  (String.to_seq input |> Seq.map int_of_char |> List.of_seq) @ [ 17; 31; 73; 47; 23 ]
;;

let make_ring len = Seq.init len (fun i -> i, i) |> Hashtbl.of_seq

let knot size ?(iterations = 1) lengths =
  let ring = make_ring size
  and offset = ref 0
  and skip = ref 0 in
  let rev offset len =
    let rec take acc pos = function
      | 0 -> acc
      | n -> take (Hashtbl.find ring pos :: acc) ((pos + 1) mod size) (n - 1)
    in
    let rec place pos = function
      | [] -> ()
      | hd :: tl ->
        Hashtbl.replace ring pos hd;
        place ((pos + 1) mod size) tl
    in
    take [] offset len |> place offset
  in
  for _ = 1 to iterations do
    List.iter
      (fun l ->
        rev !offset l;
        offset := (!offset + !skip + l) mod size;
        incr skip)
      lengths
  done;
  Hashtbl.to_seq ring
  |> List.of_seq
  |> List.sort (fun (i1, _) (i2, _) -> i1 - i2)
  |> List.map snd
;;

let part1 list = Base.List.take list 2 |> List.fold_left ( * ) 1
let hex_of_int = Printf.sprintf "%02x"

let hash list =
  let rec aux acc xor i = function
    | [] -> acc
    | hd :: tl ->
      if i = 1
      then aux acc hd (i + 1) tl
      else if i = 16
      then aux (acc ^ hex_of_int (xor lxor hd)) 0 1 tl
      else aux acc (xor lxor hd) (i + 1) tl
  in
  aux "" 0 1 list
;;

let run () =
  parse input |> knot 256 |> part1 |> print_int |> print_newline;
  ascii_parse input |> knot 256 ~iterations:64 |> hash |> print_endline
;;

(* used in later days *)
let knot_hash_of_string s = ascii_parse s |> knot 256 ~iterations:64 |> hash
