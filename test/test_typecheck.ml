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

let%expect_test "iteration 6 (booleans and more operators)" =
  let program = Helper.parse {|
    int main()
    {
      int i = 2;
      int j = 0;
      bool a = i == j;
      bool b = i < j;
      bool c = i > j;
      bool d = i <= j;
      bool e = i >= j;
      bool f = i != j;
      bool x = true;
      bool y = false;
      bool g = x == y;
      bool h = x != y;
      return j;
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
            (Some { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 2) }) };
           { Tast.bind_name = "j"; bind_type = Ast.TyInt;
             initial_value =
             (Some { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 0) }) };
           { Tast.bind_name = "a"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool;
                     expr =
                     (Tast.TBinop
                        { Tast.binop_type = Ast.Eq;
                          tbinop_left =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                          tbinop_right =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "j" })) }
                          })
                     })
             };
           { Tast.bind_name = "b"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool;
                     expr =
                     (Tast.TBinop
                        { Tast.binop_type = Ast.Less;
                          tbinop_left =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                          tbinop_right =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "j" })) }
                          })
                     })
             };
           { Tast.bind_name = "c"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool;
                     expr =
                     (Tast.TBinop
                        { Tast.binop_type = Ast.Greater;
                          tbinop_left =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                          tbinop_right =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "j" })) }
                          })
                     })
             };
           { Tast.bind_name = "d"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool;
                     expr =
                     (Tast.TBinop
                        { Tast.binop_type = Ast.Leq;
                          tbinop_left =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                          tbinop_right =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "j" })) }
                          })
                     })
             };
           { Tast.bind_name = "e"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool;
                     expr =
                     (Tast.TBinop
                        { Tast.binop_type = Ast.Geq;
                          tbinop_left =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                          tbinop_right =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "j" })) }
                          })
                     })
             };
           { Tast.bind_name = "f"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool;
                     expr =
                     (Tast.TBinop
                        { Tast.binop_type = Ast.Neq;
                          tbinop_left =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                          tbinop_right =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "j" })) }
                          })
                     })
             };
           { Tast.bind_name = "x"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool; expr = (Tast.TBoolLit true) }) };
           { Tast.bind_name = "y"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool; expr = (Tast.TBoolLit false) }) };
           { Tast.bind_name = "g"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool;
                     expr =
                     (Tast.TBinop
                        { Tast.binop_type = Ast.Eq;
                          tbinop_left =
                          { Tast.typ = Ast.TyBool;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "x" })) };
                          tbinop_right =
                          { Tast.typ = Ast.TyBool;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "y" })) }
                          })
                     })
             };
           { Tast.bind_name = "h"; bind_type = Ast.TyBool;
             initial_value =
             (Some { Tast.typ = Ast.TyBool;
                     expr =
                     (Tast.TBinop
                        { Tast.binop_type = Ast.Neq;
                          tbinop_left =
                          { Tast.typ = Ast.TyBool;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "x" })) };
                          tbinop_right =
                          { Tast.typ = Ast.TyBool;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "y" })) }
                          })
                     })
             }
           ];
         body =
         [(Tast.TReturn
             { Tast.typ = Ast.TyInt;
               expr = (Tast.TLval (Tast.TIdent { Ast.literal = "j" })) })
           ]
         }
        ]
      }
  |}]

let%expect_test "iteration 7 (if statements)" =
  let program = Helper.parse {|
    int main()
    {
      int i = 0;
      if (i > 5)
      {
        i = 10;
      }
      else
      {
        i = 2;
      }

      if (i > 5) {
        i = 5;
      }
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
         [(Tast.TIf
             { Tast.cond =
               { Tast.typ = Ast.TyBool;
                 expr =
                 (Tast.TBinop
                    { Tast.binop_type = Ast.Greater;
                      tbinop_left =
                      { Tast.typ = Ast.TyInt;
                        expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                      tbinop_right =
                      { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 5) } })
                 };
               then_br =
               (Tast.TBlock
                  { Tast.tblock_body =
                    [(Tast.TAssign
                        { Tast.tassign_left =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                          tassign_right =
                          { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 10) } })
                      ]
                    });
               else_br =
               (Tast.TBlock
                  { Tast.tblock_body =
                    [(Tast.TAssign
                        { Tast.tassign_left =
                          { Tast.typ = Ast.TyInt;
                            expr =
                            (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                          tassign_right =
                          { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 2) } })
                      ]
                    })
               });
           (Tast.TIf
              { Tast.cond =
                { Tast.typ = Ast.TyBool;
                  expr =
                  (Tast.TBinop
                     { Tast.binop_type = Ast.Greater;
                       tbinop_left =
                       { Tast.typ = Ast.TyInt;
                         expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" }))
                         };
                       tbinop_right =
                       { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 5) } })
                  };
                then_br =
                (Tast.TBlock
                   { Tast.tblock_body =
                     [(Tast.TAssign
                         { Tast.tassign_left =
                           { Tast.typ = Ast.TyInt;
                             expr =
                             (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) };
                           tassign_right =
                           { Tast.typ = Ast.TyInt; expr = (Tast.TIntLit 5) } })
                       ]
                     });
                else_br = (Tast.TBlock { Tast.tblock_body = [] }) });
           (Tast.TReturn
              { Tast.typ = Ast.TyInt;
                expr = (Tast.TLval (Tast.TIdent { Ast.literal = "i" })) })
           ]
         }
        ]
      }
  |}]
