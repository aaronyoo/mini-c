(* Typed expressions. *)
type texpr = { typ: Ast.typ; expr: aexpr } [@@deriving show]

(* Annotated expression. Expression with types. *)
and aexpr =
  | TIntLit of int
[@@deriving show]

type tstmt =
  | Return of texpr
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