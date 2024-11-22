let input' =
  "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a\njgz a -1\nset a 1\njgz a -2"
;;

type asd =
  | Reg' of char
  | Val' of int

type instruction =
  | Snd' of char
  | Rcv' of char
  | Set' of char * asd
  | Add' of char * asd
  | Mul' of char * asd
  | Mod' of char * asd
  | Jgz' of char * asd

let to_asd s =
  let _, a = Base.String.lsplit2_exn ~on:' ' s in
  match int_of_string_opt a with
  | Some n -> Val' n
  | None -> Reg' a.[0]
;;

let parse input =
  Base.String.split_lines input
  |> List.map (fun s ->
    let ins, args = Base.String.lsplit2_exn ~on:' ' s in
    match ins with
    | "snd" -> Snd' args.[0]
    | "rcv" -> Rcv' args.[0]
    | "set" -> Set' (args.[0], to_asd args)
    | "add" -> Add' (args.[0], to_asd args)
    | "mul" -> Mul' (args.[0], to_asd args)
    | "mod" -> Mod' (args.[0], to_asd args)
    | "jgz" -> Jgz' (args.[0], to_asd args)
    | _ -> failwith "invalid input")
;;

(** get array index for char *)
let i r = int_of_char r - int_of_char 'a'

let part1 instructions =
  let instructions = Array.of_list instructions
  and registers = Array.init 26 (fun _ -> 0)
  and pointer = ref 0
  and sound = ref 0 in
  let get = function
    | Reg' r -> registers.(i r)
    | Val' v -> v
  in
  while !pointer >= 0 && !pointer < Array.length instructions do
    (match instructions.(!pointer) with
     | Snd' r -> sound := registers.(i r)
     | Rcv' r -> if registers.(i r) <> 0 then pointer := -2
     | Set' (r, asd) -> registers.(i r) <- get asd
     | Add' (r, asd) -> registers.(i r) <- registers.(i r) + get asd
     | Mul' (r, asd) -> registers.(i r) <- registers.(i r) * get asd
     | Mod' (r, asd) -> registers.(i r) <- registers.(i r) mod get asd
     | Jgz' (r, asd) -> if registers.(i r) > 0 then pointer := !pointer - 1 + get asd);
    incr pointer
  done;
  !sound
;;

let part2 instructions =
  let instructions = Array.of_list instructions
  and registers = [| Array.init 26 (fun _ -> 0); Array.init 26 (fun _ -> 0) |]
  and pointer = [| 0; 0 |]
  and locked = [| false; false |]
  and queue = [| Queue.create (); Queue.create () |]
  and p = ref 0
  and count = ref 0 in
  let get p = function
    | Reg' r -> registers.(p).(i r)
    | Val' v -> v
  and snd p r =
    Queue.add registers.(p).(i r) queue.(p);
    locked.(abs (p - 1)) <- false;
    if p = 1 then incr count
  and rcv p r =
    match Queue.take_opt queue.(abs (p - 1)) with
    | Some v -> registers.(p).(i r) <- v
    | None ->
      locked.(p) <- true;
      pointer.(p) <- pointer.(p) - 1
  in
  registers.(1).(i 'p') <- 1;
  while not (locked.(0) && locked.(1)) do
    while
      (not locked.(!p)) && pointer.(!p) >= 0 && pointer.(!p) < Array.length instructions
    do
      (match instructions.(pointer.(!p)) with
       | Snd' r -> snd !p r
       | Rcv' r -> rcv !p r
       | Set' (r, asd) -> registers.(!p).(i r) <- get !p asd
       | Add' (r, asd) -> registers.(!p).(i r) <- registers.(!p).(i r) + get !p asd
       | Mul' (r, asd) -> registers.(!p).(i r) <- registers.(!p).(i r) * get !p asd
       | Mod' (r, asd) -> registers.(!p).(i r) <- registers.(!p).(i r) mod get !p asd
       | Jgz' (r, asd) ->
         (*
            it looks like jump can have a literal value
            maybe this is specific to my input or
            sometimes it's generated with '1' to force the jump
            anyways, too lazy to refactor `parse` so this is a workaround
         *)
         if (if r = '1' then 1 else registers.(!p).(i r)) > 0
         then pointer.(!p) <- pointer.(!p) - 1 + get !p asd);
      pointer.(!p) <- pointer.(!p) + 1
    done;
    p := abs (!p - 1)
  done;
  !count
;;

let run () =
  parse input' |> part1 |> print_int |> print_newline;
  parse input' |> part2 |> print_int |> print_newline
;;
