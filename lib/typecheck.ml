open Base
open Ast
open Tast

exception Error of string

type env = {
  (* Map of currently defined variables bound to their type. *)
  vars: Typ.t Map.M(String).t;
  (* Map of function names bound to their instance. *)
  funcs: Func.t Map.M(String).t;
}

let check_ident (id: string) (env: env) =
  match Map.find env.vars id with
  | None -> raise (Error ("identifier used before definition: " ^ id))
  | Some typ -> ({ typ; expr = TLval (TIdent id) }: AExpr.t)

let check_bind (b: Bind.t) (env: env): env * TBind.t =
  let vars = Map.set env.vars ~key: b.name ~data: b.typ in
  let tbind = ({
      name = b.name;
      typ = b.typ;
    }: TBind.t)
  in
  { vars; funcs = env.funcs }, tbind

let rec check_expr (expr: Expr.t) (env: env): AExpr.t =
  match expr with
  | IntLit i -> ({ typ = TyInt; expr = TIntLit i }: AExpr.t)
  | BoolLit b -> ({ typ = TyBool; expr = TBoolLit b }: AExpr.t)
  | Ident id -> check_ident id env
  | Binop (left, op, right) ->
    let left = check_expr left env in
    let right = check_expr right env in
    let expr = TExpr.TBinop (left, op, right)
    in
    (* The allowed types of the operands is dependent on the operator. *)
    let typ = (match op with
        | Add | Sub | Mul | Div ->
          (* All of these operations are arithmetic and require numbers. *)
          (match (left.typ, right.typ) with
           | (TyInt, TyInt) -> left.typ
           | _ -> raise (Error ("Operation has invalid operand types: "
                                ^ (TExpr.show expr)))
          )
        | Eq | Neq ->
          (* All of these operations operate on comparable data types. *)
          (match (left.typ, right.typ) with
           | (TyInt, TyInt) | (TyBool, TyBool) -> TyBool
           | _ -> raise (Error ("invalid binary operation" ^
                                (TExpr.show expr)))
          )
        | Less | Greater | Leq | Geq ->
          (* All of these operations operate on comparable, partial ordered types. *)
          (match (left.typ, right.typ) with
           | (TyInt, TyInt) -> TyBool
           | _ -> raise (Error ("invalid binary operation" ^
                                (TExpr.show expr)))
          ))
    in
    { typ; expr }
  | Call (callee, args) ->
    (* Look up the callee in a function table. *)
    let func = match Map.find env.funcs callee with
      | Some f -> f
      | None -> raise (Error ("callee not found: " ^ callee))
    in
    (* The type of the call expr is the type of the return value of the callee. *)
    let typ = func.typ in
    let targs = List.map args ~f:(fun arg -> check_expr arg env) in
    { typ; expr = TCall (callee, targs) }

let rec check_stmt (func: Func.t) (stmt: Stmt.t) (env: env): env * TStmt.t =
  match stmt with
  | Return e ->
    (* The returned type must match the functions declared return type. *)
    let t = check_expr e env in
    if phys_equal t.typ func.typ
    then env, TStmt.TReturn t
    else raise (Error "return type mismatch")
  | Assign (lhs, rhs) ->
    (* The left hand side must be an lvalue. *)
    let lhs = (match lhs with
        | Ident id -> check_ident id env
        | _ -> raise (Error ("invalid lvalue expression: " ^
                             (Expr.show lhs))))
    in
    let rhs = check_expr rhs env in
    if phys_equal lhs.typ rhs.typ
    then env, TAssign (lhs, rhs)
    else raise (Error ("invalid lhs and rhs types for"))
  | If (cond, t, f) ->
    let cond = check_expr cond env in
    if not (phys_equal cond.typ Typ.TyBool)
    then raise (Error ("if stmt condition is not boolean: " ^
                       (Stmt.show stmt)));
    (* There are no environment updates from if branches since they have their
       own scope. *)
    let _, t = check_stmt func t env in
    let _, f = check_stmt func f env in
    env, TIf (cond, t, f)
  | Block stmts ->
    let env, tstmts = List.fold_map stmts
        ~init: env
        ~f: (fun env stmt -> check_stmt func stmt env)
    in
    env, TStmt.TBlock tstmts
  | For (init, cond, inc, body) ->
    let env, tinit = check_stmt func init env in
    let tcond = check_expr cond env in
    if not (phys_equal tcond.typ TyBool)
    then
      raise (Error ("Expected boolean condition but got: " ^ (Typ.show tcond.typ)));
    let env, tinc = check_stmt func inc env in
    let env, tbody = check_stmt func body env in
    env, TStmt.TFor (tinit, tcond, tinc, tbody)
  | Bind bind ->
    let env, tbind = check_bind bind env in
    env, TStmt.TBind tbind

let check_func (func: Func.t) (env: env) =
  (* Add the name to the function map. *)
  let funcs = match Map.add env.funcs ~key: func.name ~data: func with
    | `Ok env -> env
    | `Duplicate -> raise (Error ("duplicate function name -- "  ^ func.name))
  in
  let env = { vars = env.vars; funcs } in

  (* Type the parameters and each statement. *)
  let working_env = env in
  let working_env, params = List.fold_map func.params
      ~init: working_env
      ~f: (fun e b -> check_bind b e)
  in
  let working_env, body = List.fold_map func.body
      ~init: working_env
      ~f: (fun e s -> check_stmt func s e)
  in
  ignore(working_env);

  (* Return the environment and typechecked function. *)
  let typ = func.typ in
  let name = func.name in
  env, ({typ; name; params; body}: TFunc.t)

let check_program (prog: Program.t) : TProgram.t =
  (* Create an empty environment. *)
  let env = {
    vars = Map.empty (module String);
    funcs = Map.empty (module String)
  } in

  (* Assign a type to each function. *)
  let env, typed_funcs = List.fold_map prog.func_decls
      ~init: env
      ~f: (fun env func -> check_func func env)
  in

  (* Check if there is a main function then output a typed program. *)
  if Map.mem env.funcs "main"
  then ({ func_decls = typed_funcs }: TProgram.t)
  else raise (Error "no main function")
