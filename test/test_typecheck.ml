open Core

let%expect_test "iteration_1" =
  let program = Helper.parse {|
    int main()
    {
      return 0;
    }
  |} in
  let typed_prog = Minicc.Typecheck.check_program program in
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Tast.TStmt.TReturn
             { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 0) })
           ]
         }
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
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Tast.TStmt.TVarDecl ({ Tast.TBind.name = "i"; typ = Ast.Typ.TyInt },
             { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 0) }));
           (Tast.TStmt.TReturn
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) })
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
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Tast.TStmt.TVarDecl ({ Tast.TBind.name = "i"; typ = Ast.Typ.TyInt },
             { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 90) }));
           (Tast.TStmt.TAssign (
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
              { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 0) }));
           (Tast.TStmt.TReturn
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) })
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
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Tast.TStmt.TVarDecl ({ Tast.TBind.name = "i"; typ = Ast.Typ.TyInt },
             { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 0) }));
           (Tast.TStmt.TAssign (
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Add,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TIntLit 3) }
                   ))
                }
              ));
           (Tast.TStmt.TAssign (
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Sub,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TIntLit 1) }
                   ))
                }
              ));
           (Tast.TStmt.TAssign (
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Div,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TIntLit 2) }
                   ))
                }
              ));
           (Tast.TStmt.TAssign (
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Mul,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TIntLit 4) }
                   ))
                }
              ));
           (Tast.TStmt.TReturn
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) })
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
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Tast.TStmt.TVarDecl ({ Tast.TBind.name = "i"; typ = Ast.Typ.TyInt },
             { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 2) }));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "j"; typ = Ast.Typ.TyInt },
              { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 0) }));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "a"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Eq,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "j")) }
                   ))
                }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "b"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Less,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "j")) }
                   ))
                }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "c"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Greater,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "j")) }
                   ))
                }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "d"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Leq,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "j")) }
                   ))
                }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "e"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Geq,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "j")) }
                   ))
                }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "f"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Neq,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "j")) }
                   ))
                }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "x"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr = (Tast.TExpr.TBoolLit true) }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "y"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr = (Tast.TExpr.TBoolLit false) }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "g"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyBool;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "x")) },
                   Ast.Op.Eq,
                   { Tast.AExpr.typ = Ast.Typ.TyBool;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "y")) }
                   ))
                }
              ));
           (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "h"; typ = Ast.Typ.TyBool },
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyBool;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "x")) },
                   Ast.Op.Neq,
                   { Tast.AExpr.typ = Ast.Typ.TyBool;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "y")) }
                   ))
                }
              ));
           (Tast.TStmt.TReturn
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "j")) })
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
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Tast.TStmt.TVarDecl ({ Tast.TBind.name = "i"; typ = Ast.Typ.TyInt },
             { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 0) }));
           (Tast.TStmt.TIf (
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Greater,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TIntLit 5) }
                   ))
                },
              (Tast.TStmt.TBlock
                 [(Tast.TStmt.TAssign (
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TIntLit 10) }
                     ))
                   ]),
              (Tast.TStmt.TBlock
                 [(Tast.TStmt.TAssign (
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TIntLit 2) }
                     ))
                   ])
              ));
           (Tast.TStmt.TIf (
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                   Ast.Op.Greater,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TIntLit 5) }
                   ))
                },
              (Tast.TStmt.TBlock
                 [(Tast.TStmt.TAssign (
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) },
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TIntLit 5) }
                     ))
                   ]),
              (Tast.TStmt.TBlock [])));
           (Tast.TStmt.TReturn
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "i")) })
           ]
         }
        ]
      }
  |}]

let%expect_test "iteration 8 (no argument function calls)" =
  let program = Helper.parse {|
    int main()
    {
      int a = 2;
      int b = 3;
      return add();
    }

    int add()
    {
      return 5;
    }
  |} in
  let typed_prog = Minicc.Typecheck.check_program program in
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "add"; params = [];
         body =
         [(Tast.TStmt.TReturn
             { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 5) })
           ]
         };
        { Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
          body =
          [(Tast.TStmt.TVarDecl ({ Tast.TBind.name = "a"; typ = Ast.Typ.TyInt },
              { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 2) }));
            (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "b"; typ = Ast.Typ.TyInt },
               { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 3) }
               ));
            (Tast.TStmt.TReturn
               { Tast.AExpr.typ = Ast.Typ.TyInt;
                 expr = (Tast.TExpr.TCall ("add", [])) })
            ]
          }
        ]
      }
  |}]

let%expect_test "iteration 8 (function call with arguments)" =
  let program = Helper.parse {|
    int main()
    {
      int a = 3;
      int b = 3;
      return add(a, b);
    }

    int add(int x, int y)
    {
      return x + y;
    }
  |} in
  let typed_prog = Minicc.Typecheck.check_program program in
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "add";
         params =
         [{ Tast.TBind.name = "x"; typ = Ast.Typ.TyInt };
           { Tast.TBind.name = "y"; typ = Ast.Typ.TyInt }];
         body =
         [(Tast.TStmt.TReturn
             { Tast.AExpr.typ = Ast.Typ.TyInt;
               expr =
               (Tast.TExpr.TBinop (
                  { Tast.AExpr.typ = Ast.Typ.TyInt;
                    expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "x")) },
                  Ast.Op.Add,
                  { Tast.AExpr.typ = Ast.Typ.TyInt;
                    expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "y")) }
                  ))
               })
           ]
         };
        { Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
          body =
          [(Tast.TStmt.TVarDecl ({ Tast.TBind.name = "a"; typ = Ast.Typ.TyInt },
              { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 3) }));
            (Tast.TStmt.TVarDecl ({ Tast.TBind.name = "b"; typ = Ast.Typ.TyInt },
               { Tast.AExpr.typ = Ast.Typ.TyInt; expr = (Tast.TExpr.TIntLit 3) }
               ));
            (Tast.TStmt.TReturn
               { Tast.AExpr.typ = Ast.Typ.TyInt;
                 expr =
                 (Tast.TExpr.TCall ("add",
                    [{ Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "a")) };
                      { Tast.AExpr.typ = Ast.Typ.TyInt;
                        expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "b")) }
                      ]
                    ))
                 })
            ]
          }
        ]
      }
  |}]

let%expect_test "factorial" =
  let program = Helper.parse {|
    int main()
    {
      return factorial(5);
    }

    int factorial(int n)
    {
      int ret;
      if (n == 0)
      {
        ret = 1;
      }
      else
      {
        ret = n * factorial(n - 1);
      }
      return ret;
    }
  |} in
  let typed_prog = Minicc.Typecheck.check_program program in
  let s = Minicc.Tast.TProgram.show typed_prog in
  printf "%s" s;
  [%expect {|
    { Tast.TProgram.func_decls =
      [{ Tast.TFunc.typ = Ast.Typ.TyInt; name = "factorial";
         params = [{ Tast.TBind.name = "n"; typ = Ast.Typ.TyInt }];
         body =
         [(Tast.TStmt.TBind { Tast.TBind.name = "ret"; typ = Ast.Typ.TyInt });
           (Tast.TStmt.TIf (
              { Tast.AExpr.typ = Ast.Typ.TyBool;
                expr =
                (Tast.TExpr.TBinop (
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "n")) },
                   Ast.Op.Eq,
                   { Tast.AExpr.typ = Ast.Typ.TyInt;
                     expr = (Tast.TExpr.TIntLit 0) }
                   ))
                },
              (Tast.TStmt.TBlock
                 [(Tast.TStmt.TAssign (
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "ret")) },
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TIntLit 1) }
                     ))
                   ]),
              (Tast.TStmt.TBlock
                 [(Tast.TStmt.TAssign (
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "ret")) },
                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                       expr =
                       (Tast.TExpr.TBinop (
                          { Tast.AExpr.typ = Ast.Typ.TyInt;
                            expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "n")) },
                          Ast.Op.Mul,
                          { Tast.AExpr.typ = Ast.Typ.TyInt;
                            expr =
                            (Tast.TExpr.TCall ("factorial",
                               [{ Tast.AExpr.typ = Ast.Typ.TyInt;
                                  expr =
                                  (Tast.TExpr.TBinop (
                                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                                       expr =
                                       (Tast.TExpr.TLval (Tast.TLval.TIdent "n"))
                                       },
                                     Ast.Op.Sub,
                                     { Tast.AExpr.typ = Ast.Typ.TyInt;
                                       expr = (Tast.TExpr.TIntLit 1) }
                                     ))
                                  }
                                 ]
                               ))
                            }
                          ))
                       }
                     ))
                   ])
              ));
           (Tast.TStmt.TReturn
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr = (Tast.TExpr.TLval (Tast.TLval.TIdent "ret")) })
           ]
         };
        { Tast.TFunc.typ = Ast.Typ.TyInt; name = "main"; params = [];
          body =
          [(Tast.TStmt.TReturn
              { Tast.AExpr.typ = Ast.Typ.TyInt;
                expr =
                (Tast.TExpr.TCall ("factorial",
                   [{ Tast.AExpr.typ = Ast.Typ.TyInt;
                      expr = (Tast.TExpr.TIntLit 5) }
                     ]
                   ))
                })
            ]
          }
        ]
      }
  |}]
