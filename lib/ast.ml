module Typ = struct
  type t = TyInt | TyBool [@@deriving show]
end

module Op = struct
  type t = Add | Sub | Mul | Div | Eq | Neq | Less | Greater | Leq | Geq
  [@@deriving show]
end

module Expr = struct
  type t =
    | IntLit of int
    | BoolLit of bool
    | Ident of string
    | Binop of t * Op.t * t
    | Call of string * t list
  [@@deriving show]
end

module Bind = struct
  type t = { typ : Typ.t; name : string } [@@deriving show]
end

module Stmt = struct
  type t =
    | Return of Expr.t
    | Assign of Expr.t * Expr.t
    | If of Expr.t * t * t
    | Block of t list
    | For of t * Expr.t * t * t
    | Bind of Bind.t
    | VarDecl of Bind.t * Expr.t
  [@@deriving show]
end

module Func = struct
  type t = {
    typ : Typ.t;
    name : string;
    params : Bind.t list;
    body : Stmt.t list;
  }
  [@@deriving show]
end

module Program = struct
  type t = { func_decls : Func.t list } [@@deriving show]
end
