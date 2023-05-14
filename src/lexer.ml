open Parser
open Sedlexing.Utf8

let whitespace = [%sedlex.regexp? Plus (' ' | '\t' | '\n')]
let comment = [%sedlex.regexp? "\\", Star (Compl '\n')]
let alphabet = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z']
let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let lower = [%sedlex.regexp? 'a' .. 'z', Star (alphabet | digit | '_')]

let rec tokenizer buf =
  match%sedlex buf with
  | whitespace -> tokenizer buf
  | comment -> tokenizer buf
  | "forall" -> FORALL
  | "lambda" -> LAMBDA
  | "let" -> LET
  | lower -> LOWER (lexeme buf)
  | "=>" -> FATARROW
  | "->" -> ARROW
  | "=" -> EQUAL
  | ":" -> COLON
  | ";" -> SEMICOLON
  | "," -> COMMA
  | "{" -> LEFT_BRACKET
  | "}" -> RIGHT_BRACKET
  | "[" -> LEFT_BRACES
  | "]" -> RIGHT_BRACES
  | "(" -> LEFT_PARENS
  | ")" -> RIGHT_PARENS
  | eof -> EOF
  | _ -> failwith "unknown token"

let provider buf () =
  let token = tokenizer buf in
  let start, stop = Sedlexing.lexing_positions buf in
  token, start, stop

let from_string parser string =
  let buf = from_string string in
  let provider = provider buf in
  MenhirLib.Convert.Simplified.traditional2revised parser provider

let from_channel parser file =
  let buf = from_channel file in
  let provider = provider buf in
  MenhirLib.Convert.Simplified.traditional2revised parser provider