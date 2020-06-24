open Core

let%expect_test "iteration_1" =
  let program = Helper.parse {|
    int main()
    {
      return 0;
    }
  |} in
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body = [(Ast.Stmt.Return (Ast.Expr.IntLit 0))] }
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
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "i" },
             (Ast.Expr.IntLit 0)));
           (Ast.Stmt.Return (Ast.Expr.Ident "i"))]
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
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "i" },
             (Ast.Expr.IntLit 90)));
           (Ast.Stmt.Assign ((Ast.Expr.Ident "i"), (Ast.Expr.IntLit 0)));
           (Ast.Stmt.Return (Ast.Expr.Ident "i"))]
         }
        ]
      }
  |}]

let%expect_test "iteration_5" =
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
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect{|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "i" },
             (Ast.Expr.IntLit 0)));
           (Ast.Stmt.Assign ((Ast.Expr.Ident "i"),
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Add,
                 (Ast.Expr.IntLit 3)))
              ));
           (Ast.Stmt.Assign ((Ast.Expr.Ident "i"),
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Sub,
                 (Ast.Expr.IntLit 1)))
              ));
           (Ast.Stmt.Assign ((Ast.Expr.Ident "i"),
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Div,
                 (Ast.Expr.IntLit 2)))
              ));
           (Ast.Stmt.Assign ((Ast.Expr.Ident "i"),
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Mul,
                 (Ast.Expr.IntLit 4)))
              ));
           (Ast.Stmt.Return (Ast.Expr.Ident "i"))]
         }
        ]
      } |}]

let%expect_test "operator precedence" =
  let program = Helper.parse {|
    int main()
    {
      int i = 3 + 1 * 5;
      return i;
    }
  |} in
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "i" },
             (Ast.Expr.Binop ((Ast.Expr.IntLit 3), Ast.Op.Add,
                (Ast.Expr.Binop ((Ast.Expr.IntLit 1), Ast.Op.Mul,
                   (Ast.Expr.IntLit 5)))
                ))
             ));
           (Ast.Stmt.Return (Ast.Expr.Ident "i"))]
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
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "i" },
             (Ast.Expr.IntLit 2)));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "j" },
              (Ast.Expr.IntLit 0)));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "a" },
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Eq,
                 (Ast.Expr.Ident "j")))
              ));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "b" },
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Less,
                 (Ast.Expr.Ident "j")))
              ));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "c" },
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Greater,
                 (Ast.Expr.Ident "j")))
              ));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "d" },
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Leq,
                 (Ast.Expr.Ident "j")))
              ));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "e" },
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Geq,
                 (Ast.Expr.Ident "j")))
              ));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "f" },
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Neq,
                 (Ast.Expr.Ident "j")))
              ));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "x" },
              (Ast.Expr.BoolLit true)));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "y" },
              (Ast.Expr.BoolLit false)));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "g" },
              (Ast.Expr.Binop ((Ast.Expr.Ident "x"), Ast.Op.Eq,
                 (Ast.Expr.Ident "y")))
              ));
           (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyBool; name = "h" },
              (Ast.Expr.Binop ((Ast.Expr.Ident "x"), Ast.Op.Neq,
                 (Ast.Expr.Ident "y")))
              ));
           (Ast.Stmt.Return (Ast.Expr.Ident "j"))]
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
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
         body =
         [(Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "i" },
             (Ast.Expr.IntLit 0)));
           (Ast.Stmt.If (
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Greater,
                 (Ast.Expr.IntLit 5))),
              (Ast.Stmt.Block
                 [(Ast.Stmt.Assign ((Ast.Expr.Ident "i"), (Ast.Expr.IntLit 10)))]),
              (Ast.Stmt.Block
                 [(Ast.Stmt.Assign ((Ast.Expr.Ident "i"), (Ast.Expr.IntLit 2)))])
              ));
           (Ast.Stmt.If (
              (Ast.Expr.Binop ((Ast.Expr.Ident "i"), Ast.Op.Greater,
                 (Ast.Expr.IntLit 5))),
              (Ast.Stmt.Block
                 [(Ast.Stmt.Assign ((Ast.Expr.Ident "i"), (Ast.Expr.IntLit 5)))]),
              (Ast.Stmt.Block [])));
           (Ast.Stmt.Return (Ast.Expr.Ident "i"))]
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
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "add"; params = [];
         body = [(Ast.Stmt.Return (Ast.Expr.IntLit 5))] };
        { Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
          body =
          [(Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "a" },
              (Ast.Expr.IntLit 2)));
            (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "b" },
               (Ast.Expr.IntLit 3)));
            (Ast.Stmt.Return (Ast.Expr.Call ("add", [])))]
          }
        ]
      }
  |}]

let%expect_test "iteration 8 (function call with argument)" =
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
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "add";
         params =
         [{ Ast.Bind.typ = Ast.Typ.TyInt; name = "x" };
           { Ast.Bind.typ = Ast.Typ.TyInt; name = "y" }];
         body =
         [(Ast.Stmt.Return
             (Ast.Expr.Binop ((Ast.Expr.Ident "x"), Ast.Op.Add,
                (Ast.Expr.Ident "y"))))
           ]
         };
        { Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
          body =
          [(Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "a" },
              (Ast.Expr.IntLit 3)));
            (Ast.Stmt.VarDecl ({ Ast.Bind.typ = Ast.Typ.TyInt; name = "b" },
               (Ast.Expr.IntLit 3)));
            (Ast.Stmt.Return
               (Ast.Expr.Call ("add",
                  [(Ast.Expr.Ident "a"); (Ast.Expr.Ident "b")])))
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
  let s = Minicc.Ast.Program.show program in
  printf "%s" s;
  [%expect {|
    { Ast.Program.func_decls =
      [{ Ast.Func.typ = Ast.Typ.TyInt; name = "factorial";
         params = [{ Ast.Bind.typ = Ast.Typ.TyInt; name = "n" }];
         body =
         [(Ast.Stmt.Bind { Ast.Bind.typ = Ast.Typ.TyInt; name = "ret" });
           (Ast.Stmt.If (
              (Ast.Expr.Binop ((Ast.Expr.Ident "n"), Ast.Op.Eq,
                 (Ast.Expr.IntLit 0))),
              (Ast.Stmt.Block
                 [(Ast.Stmt.Assign ((Ast.Expr.Ident "ret"), (Ast.Expr.IntLit 1)))
                   ]),
              (Ast.Stmt.Block
                 [(Ast.Stmt.Assign ((Ast.Expr.Ident "ret"),
                     (Ast.Expr.Binop ((Ast.Expr.Ident "n"), Ast.Op.Mul,
                        (Ast.Expr.Call ("factorial",
                           [(Ast.Expr.Binop ((Ast.Expr.Ident "n"), Ast.Op.Sub,
                               (Ast.Expr.IntLit 1)))
                             ]
                           ))
                        ))
                     ))
                   ])
              ));
           (Ast.Stmt.Return (Ast.Expr.Ident "ret"))]
         };
        { Ast.Func.typ = Ast.Typ.TyInt; name = "main"; params = [];
          body =
          [(Ast.Stmt.Return (Ast.Expr.Call ("factorial", [(Ast.Expr.IntLit 5)])))
            ]
          }
        ]
      }
  |}]
