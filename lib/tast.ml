(* Annotated expressions. *)
type aexpr = { typ: Ast.typ; expr: texpr } [@@deriving show]

and tbinop_expr = {
  binop_type: Ast.op;
  tbinop_left: aexpr;
  tbinop_right: aexpr;
} [@@deriving show]

and texpr =
  | TIntLit of int
  | TBoolLit of bool
  | TLval of tlval
  | TBinop of tbinop_expr
[@@deriving show]

and tlval =
  | TIdent of Ast.ident_expr
[@@deriving show]

type tstmt =
  | TReturn of aexpr
  | TAssign of tassign_stmt
  | TIf of tif_stmt
  | TBlock of tblock_stmt
[@@deriving show]

and tassign_stmt = {
  tassign_left: aexpr;
  tassign_right: aexpr;
} [@@deriving show]

and tif_stmt = {
  cond: aexpr;
  then_br: tstmt;
  else_br: tstmt;
} [@@deriving show]

and tblock_stmt = {
  tblock_body: tstmt list;
}

type tbind = {
  bind_name: string;
  bind_type: Ast.typ;
  initial_value: aexpr option;
} [@@deriving show]

(* Functions are the same as the untyped AST except they have typed bodies. *)
type tfunc = {
  ret_typ: Ast.typ;
  name: string;
  locals: tbind list;
  body: tstmt list;
}
[@@deriving show]

type tprog = {
  var_decls: tbind list;
  func_decls: tfunc list;
} [@@deriving show]
