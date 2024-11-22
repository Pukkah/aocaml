let input = "abba[mnop]qrst\nabcd[bddb]xyyx\naaaa[qwer]tyui\nioxxoj[asdfgh]zxcvbn"
let parse input = Util.get_lines input

let supports_TLS ip =
  let rec skip_hypernet = function
    | [] -> false, []
    | ']' :: tl -> false, tl
    | a :: b :: c :: d :: tl when a = d && b = c && a <> b -> true, tl
    | _ :: tl -> skip_hypernet tl
  in
  let rec has_abba found = function
    | [] -> found
    | '[' :: tl ->
      let abba, tl' = skip_hypernet tl in
      if abba then false else has_abba found tl'
    | a :: (b :: c :: d :: _ as tl) when a = d && b = c && a <> b -> has_abba true tl
    | _ :: tl -> has_abba found tl
  in
  has_abba false (String.to_seq ip |> List.of_seq)
;;

let supports_SSL ip =
  let abas = ref [] in
  let babs = ref [] in
  let rec process_hypernet = function
    | [] -> []
    | ']' :: tl -> tl
    | a :: (b :: c :: _ as tl) when a = c && a <> b ->
      babs := (b, a) :: !babs;
      process_hypernet tl
    | _ :: tl -> process_hypernet tl
  in
  let rec process_supernet = function
    | [] -> ()
    | '[' :: tl -> process_hypernet tl |> process_supernet
    | a :: (b :: c :: _ as tl) when a = c && a <> b ->
      abas := (a, b) :: !abas;
      process_supernet tl
    | _ :: tl -> process_supernet tl
  in
  process_supernet (String.to_seq ip |> List.of_seq);
  List.exists (fun x -> List.exists (( = ) x) !babs) !abas
;;

let run () =
  parse input |> List.filter supports_TLS |> List.length |> print_int;
  print_newline ();
  parse input |> List.filter supports_SSL |> List.length |> print_int;
  print_newline ()
;;
