open Typer

let expr_from_string str =
  let desc = Lexer.from_string Parser.term str in
  match desc with
  | Some expr -> expr
  | None -> failwith "Oh no."

let run () =
  let expr = "lambda x => let a = x; a" in
  let parse = expr_from_string expr in
  let env = Environment.empty in
  let trans = Transl.transl_expr ~env parse in
  let infer = Typer.infer ~env 0 trans |> Result.get_ok in
  Format.printf "(%a) :- %s" Printer.pp_texpr trans (Show.of_type infer)
