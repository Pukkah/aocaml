let input =
  "px{a<2006:qkq,m>2090:A,rfg}\n\
   pv{a>1716:R,A}\n\
   lnx{m>1548:A,A}\n\
   rfg{s<537:gd,x>2440:R,A}\n\
   qs{s>3448:A,lnx}\n\
   qkq{x<1416:A,crn}\n\
   crn{x>2662:A,R}\n\
   in{s<1351:px,qqz}\n\
   qqz{s>2770:qs,m<1801:hdj,R}\n\
   gd{a>3333:R,R}\n\
   hdj{m>838:A,pv}\n\n\
   {x=787,m=2655,a=1222,s=2876}\n\
   {x=1679,m=44,a=2067,s=496}\n\
   {x=2036,m=264,a=79,s=2244}\n\
   {x=2461,m=1339,a=466,s=291}\n\
   {x=2127,m=1623,a=2188,s=1013}"
;;

type part =
  { x : int
  ; m : int
  ; a : int
  ; s : int
  }

type result =
  | Accepted
  | Rejected
  | Next of string

type rule =
  | MoreThan of char * int * result
  | LessThan of char * int * result
  | Result of result

let parse_part s = Scanf.sscanf s "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s -> { x; m; a; s })

let parse_result = function
  | "A" -> Accepted
  | "R" -> Rejected
  | s -> Next s
;;

module StrMap = Map.Make (String)

let parse_rules s =
  let parse_rule = function
    | s when String.contains s '>' ->
      Scanf.sscanf s "%c>%d:%s" (fun a b c -> MoreThan (a, b, parse_result c))
    | s when String.contains s '<' ->
      Scanf.sscanf s "%c<%d:%s" (fun a b c -> LessThan (a, b, parse_result c))
    | s -> Result (parse_result s)
  in
  String.split_on_char ',' s |> List.map parse_rule
;;

let parse_workflow s =
  match String.split_on_char '{' s with
  | key :: rest :: _ -> key, parse_rules @@ String.sub rest 0 @@ (String.length rest - 1)
  | _ -> failwith "invalid workflow"
;;

let parse_input =
  let rec aux acc = function
    | "" :: tl ->
      ( List.rev acc |> List.map parse_workflow |> List.to_seq |> StrMap.of_seq
      , List.map parse_part tl )
    | hd :: tl -> aux (hd :: acc) tl
    | [] -> failwith "invalid input"
  in
  aux []
;;

let rating_of part = function
  | 'x' -> part.x
  | 'm' -> part.m
  | 'a' -> part.a
  | 's' -> part.s
  | _ -> failwith "invalid rating"
;;

let eval_part workflows part =
  let rec aux = function
    | Result Accepted :: _ -> true
    | Result Rejected :: _ -> false
    | Result (Next s) :: _ -> aux (StrMap.find s workflows)
    | MoreThan (a, b, r) :: tl ->
      if rating_of part a > b then aux [ Result r ] else aux tl
    | LessThan (a, b, r) :: tl ->
      if rating_of part a < b then aux [ Result r ] else aux tl
    | [] -> failwith "invalid rules"
  in
  aux @@ StrMap.find "in" workflows
;;

let xmas { x; m; a; s } = x + m + a + s

let part1 (workflows, parts) =
  List.filter (eval_part workflows) parts |> List.map xmas |> Util.sum
;;

let run () =
  let workflows, parts = Util.get_lines input |> parse_input in
  Printf.printf "Part 1: %d\n" @@ part1 (workflows, parts)
;;
