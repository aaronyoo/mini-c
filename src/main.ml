open Core
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d at token %s" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
    (Lexing.lexeme lexbuf)

let parse_with_error lexbuf =
  try Parser.program Lexer.read lexbuf with
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

let parse filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let program = parse_with_error lexbuf in
  ignore(Printf.printf "%s\n" (Ast.show_program program))

let command =
  Command.basic
    ~summary: "Micro-C compiler"
    ~readme: (fun () -> "More detailed information")
    Command.Param.(
      map (anon ("filename" %: string))
        ~f:(fun filename -> (fun () -> parse filename)))

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command