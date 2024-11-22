let seed = ".#.\n..#\n###"
let input' = "../.# => ##./#../...\n.#./..#/### => #..#/..../..../#..#"

let parse input =
  Base.String.substr_replace_all ~pattern:" => " ~with_:"-" input
  |> Base.String.split_lines
  |> List.map (fun s ->
    let inp, outp = Base.String.lsplit2_exn ~on:'-' s in
    String.split_on_char '/' inp, String.split_on_char '/' outp)
;;

let run () = failwith "not implemented"
