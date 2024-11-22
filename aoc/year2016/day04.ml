let input =
  "aaaaa-bbb-z-y-x-123[abxyz]\n\
   a-b-c-d-e-f-g-h-987[abcde]\n\
   not-a-real-room-404[oarel]\n\
   totally-real-room-200[decoy]"
;;

type room =
  { name : string
  ; sector : int
  ; checksum : string
  }

let parse input =
  Util.get_lines input
  |> List.map (fun s ->
    let name, meta = Base.(String.rsplit2_exn ~on:'-' s) in
    let sector', checksum' = Base.(String.lsplit2_exn ~on:'[' meta) in
    { name
    ; sector = int_of_string sector'
    ; checksum = String.sub checksum' 0 (String.length checksum' - 1)
    })
;;

let get_checksum name =
  let counts = Hashtbl.create 26 in
  let update_count x =
    if Hashtbl.mem counts x
    then Hashtbl.replace counts x (Hashtbl.find counts x + 1)
    else Hashtbl.add counts x 1
  in
  String.iter (fun c -> if c <> '-' then update_count c) name;
  let compare_counts (c1, n1) (c2, n2) =
    if n1 = n2 then Char.compare c1 c2 else n2 - n1
  in
  let sorted_counts =
    Hashtbl.fold (fun c n acc -> (c, n) :: acc) counts [] |> List.sort compare_counts
  in
  Base.List.take sorted_counts 5 |> List.map fst |> List.to_seq |> String.of_seq
;;

let decypher_name room =
  { room with
    name =
      String.map
        (fun c ->
          if c = '-'
          then ' '
          else
            Char.chr (((Char.code c - Char.code 'a' + room.sector) mod 26) + Char.code 'a'))
        room.name
  }
;;

let run () =
  let rooms = parse input in
  rooms
  |> List.filter_map (fun { name; sector; checksum } ->
    if get_checksum name = checksum then Some sector else None)
  |> List.fold_left ( + ) 0
  |> string_of_int
  |> print_endline;
  rooms
  |> List.find (fun r ->
    get_checksum r.name = r.checksum
    && (decypher_name r).name = "northpole object storage")
  |> fun room -> string_of_int room.sector |> print_endline
;;
