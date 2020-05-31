(* Annotated expressions. *)
type aexpr = { typ: Ast.typ; expr: texpr } [@@deriving show]

and texpr =
  | TIntLit of int
  | TIdent of string
[@@deriving show]

type tstmt =
  | TReturn of aexpr
[@@deriving show]

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