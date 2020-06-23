open Ast

module TLval = struct
  type t = | TIdent of string
end

module TBind = struct
  type t = {
    name: string;
    typ: Typ.t;
  } [@@deriving show]
end

(* Annotated expressions. *)
module rec AExpr: sig
  type t = {
    typ: Typ.t;
    expr: TExpr.t
  } [@@deriving show]
end = AExpr

and TExpr: sig
  type t =   | TIntLit of int
             | TBoolLit of bool
             | TLval of TLval.t
             | TBinop of AExpr.t * Op.t * AExpr.t
             | TCall of string * AExpr.t list
  [@@deriving show]
end = TExpr

module TStmt = struct
  type t =
    | TReturn of AExpr.t
    | TAssign of AExpr.t * AExpr.t
    | TIf of AExpr.t * t * t
    | TBlock of t list
    | TFor of t * AExpr.t * t * t
    | TBind of TBind.t
  [@@deriving show]
end

module TFunc = struct
  type t = {
    typ: Typ.t;
    name: string;
    params: TBind.t list;
    body: TStmt.t list;
  }
  [@@deriving show]
end

module TProgram = struct
  type t = {
    func_decls: TFunc.t list;
  } [@@deriving show]
end
