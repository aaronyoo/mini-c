type typ = TyInt [@@deriving show]

type expr =
  | IntLit of int
  | Ident of string
[@@deriving show]

type bind = {
  bind_type: typ;
  bind_name: string;
  initial_value: expr option;
}
[@@deriving show]

type stmt =
  | Return of expr
[@@deriving show]

type func = {
  ret_typ: typ;
  name: string;
  locals: bind list;
  body: stmt list;
} [@@deriving show]

type program = {
  var_decls: bind list;
  func_decls: func list;
} [@@deriving show]