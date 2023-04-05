open Icpc_domestic2021_c

let rec read_input () =
  let str = read_line () in
  match str with "-1" -> [] | _ -> str :: read_input ()

let () =
  read_input ()
  |> List.map (fun exp_str ->
         exp_str |> Lexing.from_string |> Parser.root Lexer.main |> Solver.solve)
  |> List.map string_of_int |> String.concat "\n" |> print_endline
