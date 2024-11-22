let input =
  "eedadn\n\
   drvtee\n\
   eandsr\n\
   raavrd\n\
   atevrs\n\
   tsrnev\n\
   sdttsa\n\
   rasrtv\n\
   nssdts\n\
   ntnada\n\
   svetve\n\
   tesnvt\n\
   vntsnd\n\
   vrdear\n\
   dvrsen\n\
   enarar"
;;

let recover_char ~asc name =
  let counts = Hashtbl.create 26 in
  let update_count x =
    if Hashtbl.mem counts x
    then Hashtbl.replace counts x (Hashtbl.find counts x + 1)
    else Hashtbl.add counts x 1
  in
  String.iter update_count name;
  let sorted_counts =
    Hashtbl.to_seq counts
    |> List.of_seq
    |> List.sort (fun (_, a) (_, b) -> if asc then b - a else a - b)
  in
  List.hd sorted_counts |> fst
;;

let recover ~asc input =
  let lines = Util.get_lines input in
  let columns =
    List.init (String.length (List.hd lines)) (fun i -> List.map (fun l -> l.[i]) lines)
  in
  let recovered_chars =
    List.map (fun c -> recover_char ~asc (String.of_seq (List.to_seq c))) columns
  in
  String.of_seq (List.to_seq recovered_chars)
;;

let run () =
  recover ~asc:true input |> print_endline;
  recover ~asc:false input |> print_endline
;;
