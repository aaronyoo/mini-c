type typ = TyInt [@@deriving show]

type ident_expr = {
  literal: string
} [@@deriving show]

type assign_stmt = {
  left: expr;
  right: expr;
} [@@deriving show]

and expr =
  | IntLit of int
  | Ident of ident_expr
[@@deriving show]

type bind = {
  bind_type: typ;
  bind_name: string;
  initial_value: expr option;
}
[@@deriving show]

type stmt =
  | Return of expr
  | Assign of assign_stmt
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