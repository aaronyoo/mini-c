(* Annotated expressions. *)
type aexpr = { typ: Ast.typ; expr: texpr } [@@deriving show]

and texpr =
  | TIntLit of int
[@@deriving show]

type tstmt =
  | TReturn of aexpr
[@@deriving show]

(* Functions are the same as the untyped AST except they have typed bodies *)
type tfunc = {
  ret_typ: Ast.typ;
  name: string;
  body: tstmt list;
}
[@@deriving show]

type tprog = {
  func_decls: tfunc list;
} [@@deriving show]