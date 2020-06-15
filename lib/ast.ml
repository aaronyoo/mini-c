type typ = TyInt
         | TyBool
[@@deriving show]

type op = Add
        | Sub
        | Mul
        | Div
        | Eq
        | Neq
        | Less
        | Greater
        | Leq
        | Geq
[@@deriving show]

type expr =
  | IntLit of int
  | BoolLit of bool
  | Ident of ident_expr
  | Binop of binop_expr
  | Call of call_expr
[@@deriving show]

and ident_expr = {
  literal: string
} [@@deriving show]

and binop_expr = {
  binop_type: op;
  binop_left: expr;
  binop_right: expr;
} [@@deriving show]

and call_expr = {
  callee: string;
  args: expr list;
} [@@deriving show]

type bind = {
  bind_type: typ;
  bind_name: string;
  initial_value: expr option;
}
[@@deriving show]

type assign_stmt = {
  assign_left: expr;
  assign_right: expr;
} [@@deriving show]

type if_stmt = {
  cond: expr;
  then_br: stmt;
  else_br: stmt;
} [@@deriving show]

and block_stmt = {
  block_body: stmt list;
} [@@deriving show]

and stmt =
  | Return of expr
  | Assign of assign_stmt
  | If of if_stmt
  | Block of block_stmt
[@@deriving show]

type func = {
  ret_typ: typ;
  name: string;
  params: bind list;
  locals: bind list;
  body: stmt list;
} [@@deriving show]

type program = {
  var_decls: bind list;
  func_decls: func list;
} [@@deriving show]
