type typ = TyInt [@@deriving show]
type expr = IntLit of int [@@deriving show]
type stmt = Return of expr [@@deriving show]

type func_decl = {
  ret_typ: typ;
  name: string;
  body: stmt list;
} [@@deriving show]

type program = {
  func_decls: func_decl list;
} [@@deriving show]