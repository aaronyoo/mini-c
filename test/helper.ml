open Core
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d at token %s" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
    (Lexing.lexeme lexbuf)

let parse_with_error lexbuf =
  try Minicc.Parser.program Minicc.Lexer.read lexbuf with
  | Minicc.Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse s =
  let lexbuf = Lexing.from_string s in
  parse_with_error lexbuf
