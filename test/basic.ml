module To_test = struct
  let typecheck = Minicc.Typecheck.check_program
end

let test_typecheck_no_main () =
  let lexbuf = Lexing.from_string "int j() {}" in
  let program = Minicc.Parser.program Minicc.Lexer.read lexbuf in
  Alcotest.check_raises "throws exception"
    (Minicc.Typecheck.Error "no main function")
    (fun () -> ignore(Minicc.Typecheck.check_program program))

let test_typecheck_duplicate_names () =
  let lexbuf = Lexing.from_string "int main() {} int main() {}" in
  let program = Minicc.Parser.program Minicc.Lexer.read lexbuf in
  Alcotest.check_raises "throws exception"
    (Minicc.Typecheck.Error "duplicate function name -- main")
    (fun () -> ignore(Minicc.Typecheck.check_program program))

let () =
  Alcotest.run "Basic" [
    "check_program", [
      Alcotest.test_case "no main" `Quick test_typecheck_no_main;
      Alcotest.test_case "duplicate function names" `Quick test_typecheck_duplicate_names;
    ];
  ]