open Core

let%expect_test "iteration_1" =
  let program = Helper.parse {|
    int main()
    {
      return 0;
    }
  |} in
  let s = Minicc.Ast.show_program program in
  printf "%s" s;
  [%expect {|
    { Ast.var_decls = [];
      func_decls =
      [{ Ast.ret_typ = Ast.TyInt; name = "main"; locals = [];
         body = [(Ast.Return (Ast.IntLit 0))] }
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
  let s = Minicc.Ast.show_program program in
  printf "%s" s;
  [%expect {|
    { Ast.var_decls = [];
      func_decls =
      [{ Ast.ret_typ = Ast.TyInt; name = "main";
         locals =
         [{ Ast.bind_type = Ast.TyInt; bind_name = "i";
            initial_value = (Some (Ast.IntLit 0)) }
           ];
         body = [(Ast.Return (Ast.Ident { Ast.literal = "i" }))] }
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
  let s = Minicc.Ast.show_program program in
  printf "%s" s;
  [%expect {|
    { Ast.var_decls = [];
      func_decls =
      [{ Ast.ret_typ = Ast.TyInt; name = "main";
         locals =
         [{ Ast.bind_type = Ast.TyInt; bind_name = "i";
            initial_value = (Some (Ast.IntLit 90)) }
           ];
         body =
         [(Ast.Assign
             { Ast.assign_left = (Ast.Ident { Ast.literal = "i" });
               assign_right = (Ast.IntLit 0) });
           (Ast.Return (Ast.Ident { Ast.literal = "i" }))]
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
  let s = Minicc.Ast.show_program program in
  printf "%s" s;
  [%expect{|
    { Ast.var_decls = [];
      func_decls =
      [{ Ast.ret_typ = Ast.TyInt; name = "main";
         locals =
         [{ Ast.bind_type = Ast.TyInt; bind_name = "i";
            initial_value = (Some (Ast.IntLit 0)) }
           ];
         body =
         [(Ast.Assign
             { Ast.assign_left = (Ast.Ident { Ast.literal = "i" });
               assign_right =
               (Ast.Binop
                  { Ast.binop_type = Ast.Add;
                    binop_left = (Ast.Ident { Ast.literal = "i" });
                    binop_right = (Ast.IntLit 3) })
               });
           (Ast.Assign
              { Ast.assign_left = (Ast.Ident { Ast.literal = "i" });
                assign_right =
                (Ast.Binop
                   { Ast.binop_type = Ast.Sub;
                     binop_left = (Ast.Ident { Ast.literal = "i" });
                     binop_right = (Ast.IntLit 1) })
                });
           (Ast.Assign
              { Ast.assign_left = (Ast.Ident { Ast.literal = "i" });
                assign_right =
                (Ast.Binop
                   { Ast.binop_type = Ast.Div;
                     binop_left = (Ast.Ident { Ast.literal = "i" });
                     binop_right = (Ast.IntLit 2) })
                });
           (Ast.Assign
              { Ast.assign_left = (Ast.Ident { Ast.literal = "i" });
                assign_right =
                (Ast.Binop
                   { Ast.binop_type = Ast.Mul;
                     binop_left = (Ast.Ident { Ast.literal = "i" });
                     binop_right = (Ast.IntLit 4) })
                });
           (Ast.Return (Ast.Ident { Ast.literal = "i" }))]
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
  let s = Minicc.Ast.show_program program in
  printf "%s" s;
  [%expect {|
    { Ast.var_decls = [];
      func_decls =
      [{ Ast.ret_typ = Ast.TyInt; name = "main";
         locals =
         [{ Ast.bind_type = Ast.TyInt; bind_name = "i";
            initial_value =
            (Some (Ast.Binop
                     { Ast.binop_type = Ast.Add; binop_left = (Ast.IntLit 3);
                       binop_right =
                       (Ast.Binop
                          { Ast.binop_type = Ast.Mul;
                            binop_left = (Ast.IntLit 1);
                            binop_right = (Ast.IntLit 5) })
                       }))
            }
           ];
         body = [(Ast.Return (Ast.Ident { Ast.literal = "i" }))] }
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
  let s = Minicc.Ast.show_program program in
  printf "%s" s;
  [%expect {|
    { Ast.var_decls = [];
      func_decls =
      [{ Ast.ret_typ = Ast.TyInt; name = "main";
         locals =
         [{ Ast.bind_type = Ast.TyInt; bind_name = "i";
            initial_value = (Some (Ast.IntLit 2)) };
           { Ast.bind_type = Ast.TyInt; bind_name = "j";
             initial_value = (Some (Ast.IntLit 0)) };
           { Ast.bind_type = Ast.TyBool; bind_name = "a";
             initial_value =
             (Some (Ast.Binop
                      { Ast.binop_type = Ast.Eq;
                        binop_left = (Ast.Ident { Ast.literal = "i" });
                        binop_right = (Ast.Ident { Ast.literal = "j" }) }))
             };
           { Ast.bind_type = Ast.TyBool; bind_name = "b";
             initial_value =
             (Some (Ast.Binop
                      { Ast.binop_type = Ast.Less;
                        binop_left = (Ast.Ident { Ast.literal = "i" });
                        binop_right = (Ast.Ident { Ast.literal = "j" }) }))
             };
           { Ast.bind_type = Ast.TyBool; bind_name = "c";
             initial_value =
             (Some (Ast.Binop
                      { Ast.binop_type = Ast.Greater;
                        binop_left = (Ast.Ident { Ast.literal = "i" });
                        binop_right = (Ast.Ident { Ast.literal = "j" }) }))
             };
           { Ast.bind_type = Ast.TyBool; bind_name = "d";
             initial_value =
             (Some (Ast.Binop
                      { Ast.binop_type = Ast.Leq;
                        binop_left = (Ast.Ident { Ast.literal = "i" });
                        binop_right = (Ast.Ident { Ast.literal = "j" }) }))
             };
           { Ast.bind_type = Ast.TyBool; bind_name = "e";
             initial_value =
             (Some (Ast.Binop
                      { Ast.binop_type = Ast.Geq;
                        binop_left = (Ast.Ident { Ast.literal = "i" });
                        binop_right = (Ast.Ident { Ast.literal = "j" }) }))
             };
           { Ast.bind_type = Ast.TyBool; bind_name = "f";
             initial_value =
             (Some (Ast.Binop
                      { Ast.binop_type = Ast.Neq;
                        binop_left = (Ast.Ident { Ast.literal = "i" });
                        binop_right = (Ast.Ident { Ast.literal = "j" }) }))
             };
           { Ast.bind_type = Ast.TyBool; bind_name = "x";
             initial_value = (Some (Ast.BoolLit true)) };
           { Ast.bind_type = Ast.TyBool; bind_name = "y";
             initial_value = (Some (Ast.BoolLit false)) };
           { Ast.bind_type = Ast.TyBool; bind_name = "g";
             initial_value =
             (Some (Ast.Binop
                      { Ast.binop_type = Ast.Eq;
                        binop_left = (Ast.Ident { Ast.literal = "x" });
                        binop_right = (Ast.Ident { Ast.literal = "y" }) }))
             };
           { Ast.bind_type = Ast.TyBool; bind_name = "h";
             initial_value =
             (Some (Ast.Binop
                      { Ast.binop_type = Ast.Neq;
                        binop_left = (Ast.Ident { Ast.literal = "x" });
                        binop_right = (Ast.Ident { Ast.literal = "y" }) }))
             }
           ];
         body = [(Ast.Return (Ast.Ident { Ast.literal = "j" }))] }
        ]
      }
  |}]
