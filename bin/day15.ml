let input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

let hash str =
  Seq.fold_left (fun acc c -> (acc + Char.code c) * 17 mod 256) 0 @@ String.to_seq str
;;

module BoxMap = Map.Make (Int)

module OrdMap = struct
  module StringMap = Map.Make (String)

  type t =
    { map : int StringMap.t
    ; order : string list
    }

  let empty = { map = StringMap.empty; order = [] }

  let add key value om =
    if StringMap.mem key om.map
    then { om with map = StringMap.add key value om.map }
    else { map = StringMap.add key value om.map; order = key :: om.order }
  ;;

  let remove key om =
    { map = StringMap.remove key om.map; order = List.filter (( <> ) key) om.order }
  ;;

  let mapi f om =
    om.order
    |> List.rev
    |> List.mapi (fun i key ->
      let value = StringMap.find key om.map in
      f i key value)
  ;;
end

let part1 input =
  input |> String.split_on_char ',' |> List.map hash |> List.fold_left ( + ) 0
;;

let box_power ?(debug = false) box_nr lenses =
  OrdMap.mapi
    (fun lens_nr label focal_len ->
      let power = (1 + box_nr) * (1 + lens_nr) * focal_len in
      if debug
      then
        Printf.printf
          "%s: %d * %d * %d = %d\n"
          label
          (box_nr + 1)
          (lens_nr + 1)
          focal_len
          power;
      power)
    lenses
  |> List.fold_left ( + ) 0
;;

let part2 ?(debug = false) input =
  let boxes = ref BoxMap.empty in
  input
  |> String.split_on_char ','
  |> List.iter (fun s ->
    if String.contains s '-'
    then (
      let label = String.sub s 0 (String.index s '-') in
      let box_nr = hash label in
      match BoxMap.find_opt box_nr !boxes with
      | Some box -> boxes := BoxMap.add box_nr (OrdMap.remove label box) !boxes
      | None -> ())
    else (
      let label = String.sub s 0 (String.index s '=') in
      let box_nr = hash label in
      let value =
        String.sub s (String.index s '=' + 1) (String.length s - String.index s '=' - 1)
        |> int_of_string
      in
      match BoxMap.find_opt box_nr !boxes with
      | Some box -> boxes := BoxMap.add box_nr (OrdMap.add label value box) !boxes
      | None -> boxes := BoxMap.add box_nr (OrdMap.empty |> OrdMap.add label value) !boxes));
  BoxMap.fold (fun box_nr lenses acc -> acc + box_power ~debug box_nr lenses) !boxes 0
;;

let run () =
  Printf.printf "Day 14: %d\n" (part1 input);
  Printf.printf "Day 14: %d\n" (part2 input)
;;
