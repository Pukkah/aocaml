let input = "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a"

type cm =
  | Broadcaster
  | FlipFlop
  | Conjuction

module StrMap = Map.Make (String)

let parse_input input =
  let parse_module = function
    | s when String.contains s '%' -> FlipFlop, String.sub s 1 (String.length s - 1)
    | s when String.contains s '&' -> Conjuction, String.sub s 1 (String.length s - 1)
    | s -> Broadcaster, s
  and parse_connections s = String.split_on_char ',' s |> List.map String.trim in
  Util.get_lines input
  |> List.map (fun s ->
    Scanf.sscanf s "%s -> %s" (fun a b ->
      let cm, key = parse_module a in
      key, (cm, parse_connections b)))
  |> List.to_seq
  |> StrMap.of_seq
;;

let run () = print_string input
