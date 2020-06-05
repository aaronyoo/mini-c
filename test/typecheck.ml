open Core

let%expect_test "iteration_1" =
  let program = Helper.parse {|
    int main()
    {
      return 0;
    }
  |} in
  let typed_prog = Minicc.Typecheck.check_program program in
  let s = Minicc.Tast.show_tprog typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.var_decls = [];
      func_decls =
      [{ Tast.ret_typ = Ast.TyInt; name = "main"; locals = [];
         body =
         [(Tast.TReturn { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 0) })] }
        ]
      }
  |}]

let%expect_test "iteration_2" =
  let program = Helper.parse {|
    int main()
    {
      int i = 0;
      return i;
    }
  |} in
  let typed_prog = Minicc.Typecheck.check_program program in
  let s = Minicc.Tast.show_tprog typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.var_decls = [];
      func_decls =
      [{ Tast.ret_typ = Ast.TyInt; name = "main";
         locals =
         [{ Tast.bind_name = "i"; bind_type = Ast.TyInt;
            initial_value =
            (Some { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 0) }) }
           ];
         body =
         [(Tast.TReturn
             { Tast.typ = Ast.TyInt;
               expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) })
           ]
         }
        ]
      }
  |}]

let%expect_test "iteration_3" =
  let program = Helper.parse {|
    int main()
    {
      int i = 90;
      i = 0;
      return i;
    }
  |} in
  let typed_prog = Minicc.Typecheck.check_program program in
  let s = Minicc.Tast.show_tprog typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.var_decls = [];
      func_decls =
      [{ Tast.ret_typ = Ast.TyInt; name = "main";
         locals =
         [{ Tast.bind_name = "i"; bind_type = Ast.TyInt;
            initial_value =
            (Some { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 90) }) }
           ];
         body =
         [(Tast.TAssign
             { Tast.left =
               { Tast.typ = Ast.TyInt;
                 expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
               right = { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 0) } });
           (Tast.TReturn
              { Tast.typ = Ast.TyInt;
                expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) })
           ]
         }
        ]
      }
  |}]
