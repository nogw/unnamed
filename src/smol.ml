(* type testable = { name : string; expected : Tree.poly; received : Tree.poly } *)

let run () =
  let parse = "lambda x => lambda y => x" |> Lexer.from_string Parser.term |> Option.get in
  let typed = Typer.infer ~ctx:Typer.Context.empty_name 0 parse |> Result.get_ok in
  Format.printf "(%a) -: %s\n" Printer.pp_expr parse (Typer.Show.of_type typed)