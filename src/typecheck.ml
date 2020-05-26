open Base

exception Error of string

type funcs = Ast.func Map.M(String).t
type env = {
  funcs: funcs;
}

let check_expr (expr: Ast.expr) =
  match expr with
  | Ast.IntLit i -> ({ typ = Ast.TyInt; expr = Tast.TIntLit i }: Tast.texpr)

let check_stmt (func: Ast.func) (stmt: Ast.stmt) =
  match stmt with
  | Ast.Return e ->
    (* The returned type must match the functions declared return type. *)
    let t = check_expr e in
    if phys_equal t.typ func.ret_typ
    then Tast.Return t
    else raise (Error "Return Type Mismatch")

let check_func (func: Ast.func) (env: env) =
  (* Make sure that there are not overlapping function names. *)
  if Map.mem env.funcs func.name
  then raise (Error ("Duplicate Function Name: "  ^ func.name))
  else
    (* Add name to function map *)
    let funcs = Map.set env.funcs ~key: func.name ~data: func in
    (* Type each statement. *)
    let typed_stmts = List.map func.body ~f: (fun s -> check_stmt func s) in
    { funcs } , ({ret_typ = func.ret_typ; name = func.name; body = typed_stmts}: Tast.tfunc)

let check_program (prog: Ast.program) : Tast.tprog =
  (* Type each function. *)
  let env = { funcs = Map.empty (module String) } in
  let env, typed_funcs = List.fold_map prog.func_decls
      ~f: (fun e f -> check_func f e)
      ~init: env
  in
  if Map.mem env.funcs "main"
  then ({ func_decls = typed_funcs }: Tast.tprog)
  else raise (Error "No main Function")