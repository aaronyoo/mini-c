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

let parse filename =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  let program = parse_with_error lexbuf in
  try
    let typed_prog = Minicc.Typecheck.check_program program in
    Printf.printf "%s\n\n" (Minicc.Tast.show_tprog typed_prog);
    Minicc.Codegen.gen_program typed_prog;
    Llvm.dump_module Minicc.Codegen.the_module;
    ignore(Llvm_bitwriter.write_bitcode_file Minicc.Codegen.the_module "/tmp/test.bc");
  with
  | Minicc.Typecheck.Error e -> fprintf stderr "typecheck error: %s\n" e;
    exit (-1)
  | Minicc.Codegen.Error e -> fprintf stderr "codegen error: %s\n" e;
    exit(-1)

let command =
  Command.basic
    ~summary: "Micro-C compiler"
    ~readme: (fun () -> "More detailed information")
    Command.Param.(
      map (anon ("filename" %: string))
        ~f:(fun filename -> (fun () -> parse filename)))

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command