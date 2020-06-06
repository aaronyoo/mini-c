type typ = TyInt [@@deriving show]

type op = Add
        | Sub
        | Mul
        | Div
[@@deriving show]

type ident_expr = {
  literal: string
} [@@deriving show]

type binop_expr = {
  binop_type: op;
  binop_left: expr;
  binop_right: expr;
} [@@deriving show]

and assign_stmt = {
  assign_left: expr;
  assign_right: expr;
} [@@deriving show]

and expr =
  | IntLit of int
  | Ident of ident_expr
  | Binop of binop_expr
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
