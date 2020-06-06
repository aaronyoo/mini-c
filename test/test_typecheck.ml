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
             { Tast.tassign_left =
               { Tast.typ = Ast.TyInt;
                 expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
               tassign_right = { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 0) }
               });
           (Tast.TReturn
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
      int i = 0;
      i = i + 3;
      i = i - 1;
      i = i / 2;
      i = i * 4;
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
         [(Tast.TAssign
             { Tast.tassign_left =
               { Tast.typ = Ast.TyInt;
                 expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
               tassign_right =
               { Tast.typ = Ast.TyInt;
                 expr =
                 (Tast.TBinop
                    { Tast.binop_type = Ast.Add;
                      tbinop_left =
                      { Tast.typ = Ast.TyInt;
                        expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                      tbinop_right =
                      { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 3) } })
                 }
               });
           (Tast.TAssign
              { Tast.tassign_left =
                { Tast.typ = Ast.TyInt;
                  expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                tassign_right =
                { Tast.typ = Ast.TyInt;
                  expr =
                  (Tast.TBinop
                     { Tast.binop_type = Ast.Sub;
                       tbinop_left =
                       { Tast.typ = Ast.TyInt;
                         expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" }))
                         };
                       tbinop_right =
                       { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 1) } })
                  }
                });
           (Tast.TAssign
              { Tast.tassign_left =
                { Tast.typ = Ast.TyInt;
                  expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                tassign_right =
                { Tast.typ = Ast.TyInt;
                  expr =
                  (Tast.TBinop
                     { Tast.binop_type = Ast.Div;
                       tbinop_left =
                       { Tast.typ = Ast.TyInt;
                         expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" }))
                         };
                       tbinop_right =
                       { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 2) } })
                  }
                });
           (Tast.TAssign
              { Tast.tassign_left =
                { Tast.typ = Ast.TyInt;
                  expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                tassign_right =
                { Tast.typ = Ast.TyInt;
                  expr =
                  (Tast.TBinop
                     { Tast.binop_type = Ast.Mul;
                       tbinop_left =
                       { Tast.typ = Ast.TyInt;
                         expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" }))
                         };
                       tbinop_right =
                       { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 4) } })
                  }
                });
           (Tast.TReturn
              { Tast.typ = Ast.TyInt;
                expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) })
           ]
         }
        ]
      }
  |}]
