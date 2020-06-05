open Base
open Ast
open Tast

exception Error of string

type env = {
  (* Map of currently defined variables bound to their type. *)
  vars: typ Map.M(String).t;
  (* Map of function names bound to their instance *)
  funcs: func Map.M(String).t;
}

let check_ident_expr (id_expr: ident_expr) (env: env) =
  match Map.find env.vars id_expr.literal with
  | None -> raise (Error ("identifier used before definition: " ^ id_expr.literal))
  | Some typ -> ({ typ; expr = TLval (TIdent id_expr) }: aexpr)

let check_expr (expr: expr) (env: env) =
  match expr with
  | IntLit i -> ({ typ = TyInt; expr = TIntLit i }: aexpr)
  | Ident id -> check_ident_expr id env

let check_assign_stmt (stmt: assign_stmt) (env: env) =
  (* Type both the left and right hand sides. Also the left hand side must be an lvalue. *)
  let left_aexpr = match stmt.left with
    | Ident id_expr -> check_ident_expr id_expr env
    | _ -> raise (Error ("invalid lvalue expression: \n" ^ (show_expr stmt.left)))
  in
  let right_aexpr = check_expr stmt.right env in
  (* The left and right sides must have the same type. *)
  if phys_equal left_aexpr.typ right_aexpr.typ
  then ({ left = left_aexpr; right = right_aexpr }: tassign_stmt)
  else raise (Error ("invalid assignment: "
                     ^ (show_aexpr left_aexpr)
                     ^ " = " ^ (show_aexpr right_aexpr)))

let check_stmt (func: func) (stmt: stmt) (env: env) =
  match stmt with
  | Return e ->
    (* The returned type must match the functions declared return type. *)
    let t = check_expr e env in
    if phys_equal t.typ func.ret_typ
    then TReturn t
    else raise (Error "return type mismatch")
  | Assign assign_stmt -> TAssign (check_assign_stmt assign_stmt env)

let check_bind (b: bind) (env: env) =
  let vars = Map.set env.vars ~key: b.bind_name ~data: b.bind_type in
  let tbind = ({
      bind_name = b.bind_name;
      bind_type = b.bind_type;
      initial_value = match b.initial_value with
        | None -> None
        | Some v -> Some(check_expr v env)
    }: tbind)
  in
  { vars; funcs = env.funcs }, tbind

let check_func (func: func) (env: env) =
  (* Save variables to be restored at the end *)
  let saved_vars = env.vars in
  (* Make sure that there are no overlapping function names. *)
  if Map.mem env.funcs func.name
  then raise (Error ("duplicate function name -- "  ^ func.name))
  else
    (** Add name to function map *)
    let funcs = Map.set env.funcs ~key: func.name ~data: func in
    (** Add each variable to the environment *)
    let env, var_decls = List.fold_map func.locals
        ~init: env
        ~f: (fun env b -> check_bind b env)
    in
    (* Type each statement. *)
    let typed_stmts = List.map func.body ~f: (fun s -> check_stmt func s env) in
    { vars = saved_vars; funcs } ,
    ({ret_typ = func.ret_typ; name = func.name; locals = var_decls; body = typed_stmts}: tfunc)

let check_program (prog: program) : tprog =
  (* Create an empty environment. *)
  let env = {
    vars = Map.empty (module String);
    funcs = Map.empty (module String)
  } in
  (* Assign a type to each global variable. *)
  let env, typed_globals = List.fold_map prog.var_decls
      ~init: env
      ~f: (fun e v -> check_bind v e)
  in
  (* Assign a type to each function. *)
  let env, typed_funcs = List.fold_map prog.func_decls
      ~init: env
      ~f: (fun e f -> check_func f e)
  in
  (* Check if there is a main function then output a typed program. *)
  if Map.mem env.funcs "main"
  then ({ var_decls = typed_globals; func_decls = typed_funcs }: tprog)
  else raise (Error "no main function")
