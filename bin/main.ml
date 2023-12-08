let () =
  let day = Sys.argv.(1) in
  print_endline ("\nAdvent of Code 2023 - Day " ^ day);
  match int_of_string_opt day with
  | Some 1 -> Day01.run ()
  | Some 2 -> Day02.run ()
  | Some 6 -> Day06.run ()
  | Some 7 -> Day07.run ()
  | Some 8 -> Day08.run ()
  | _ -> failwith "Invalid day"
;;
