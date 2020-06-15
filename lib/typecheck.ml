open Base
open Ast
open Tast

exception Error of string

type env = {
  (* Map of currently defined variables bound to their type. *)
  vars: typ Map.M(String).t;
  (* Map of function names bound to their instance. *)
  funcs: func Map.M(String).t;
}

let check_ident_expr (id_expr: ident_expr) (env: env) =
  match Map.find env.vars id_expr.literal with
  | None -> raise (Error ("identifier used before definition: " ^ id_expr.literal))
  | Some typ -> ({ typ; expr = TLval (TIdent id_expr) }: aexpr)

let rec check_expr (expr: expr) (env: env) =
  match expr with
  | IntLit i -> ({ typ = TyInt; expr = TIntLit i }: aexpr)
  | BoolLit b -> ({ typ = TyBool; expr = TBoolLit b }: aexpr)
  | Ident id -> check_ident_expr id env
  | Binop b -> check_binop_expr b env
  | Call c -> check_call_expr c env

and check_binop_expr (binop_expr: binop_expr) (env: env) =
  let left = check_expr binop_expr.binop_left env in
  let right = check_expr binop_expr.binop_right env in
  let expr = TBinop {
      binop_type = binop_expr.binop_type;
      tbinop_left = left;
      tbinop_right = right; }
  in
  (* The allowed types of the operands is dependent on the operator. *)
  match binop_expr.binop_type with
  | Add | Sub | Mul | Div ->
    (* All of these operations are arithmetic and require numbers. *)
    (match (left.typ, right.typ) with
     | (TyInt, TyInt) -> let typ = left.typ in { typ; expr }
     | _ -> raise (Error ("Operation has invalid operand types: "
                          ^ (show_binop_expr binop_expr)))
    )
  | Eq | Neq ->
    (* All of these operations operate on comparable data types. *)
    (match (left.typ, right.typ) with
     | (TyInt, TyInt) | (TyBool, TyBool) -> let typ = TyBool in { typ; expr }
     | _ -> raise (Error ("invalid binary operation" ^
                          (show_binop_expr binop_expr)))
    )
  | Less | Greater | Leq | Geq ->
    (* All of these operations operate on comparable, partial ordered types. *)
    (match (left.typ, right.typ) with
     | (TyInt, TyInt) -> let typ = TyBool in { typ; expr }
     | _ -> raise (Error ("invalid binary operation" ^
                          (show_binop_expr binop_expr)))
    )

and check_call_expr (call_expr: call_expr) (env: env) =
  (* Look up the callee in a function table. *)
  let func = match Map.find env.funcs call_expr.callee with
    | Some f -> f
    | None -> raise (Error ("callee not found: " ^ call_expr.callee))
  in
  (* The type of the call expr is the type of the return value of the callee. *)
  let typ = func.ret_typ in
  let tcallee = call_expr.callee in
  (* Type each one of the arguments. *)
  let targs = List.map call_expr.args ~f:(fun arg -> check_expr arg env) in
  let tcall = ({ tcallee; targs }: tcall_expr) in
  ({ typ; expr = TCall tcall }: aexpr)

let rec check_stmt (func: func) (stmt: stmt) (env: env) =
  match stmt with
  | Return e ->
    (* The returned type must match the functions declared return type. *)
    let t = check_expr e env in
    if phys_equal t.typ func.ret_typ
    then TReturn t
    else raise (Error "return type mismatch")
  | Assign assign_stmt -> TAssign (check_assign_stmt assign_stmt env)
  | If if_stmt -> TIf (check_if_stmt func if_stmt env)
  | Block block_stmt -> TBlock (check_block_stmt func block_stmt env)

and check_assign_stmt (stmt: assign_stmt) (env: env) =
  (* Type both the left and right hand sides. Also the left hand side must be an lvalue. *)
  let left_aexpr = match stmt.assign_left with
    | Ident id_expr -> check_ident_expr id_expr env
    | _ -> raise (Error ("invalid lvalue expression: \n" ^ (show_expr stmt.assign_left)))
  in
  let right_aexpr = check_expr stmt.assign_right env in
  (* The left and right sides must have the same type. *)
  if phys_equal left_aexpr.typ right_aexpr.typ
  then ({ tassign_left = left_aexpr; tassign_right = right_aexpr }: tassign_stmt)
  else raise (Error ("invalid assignment: "
                     ^ (show_aexpr left_aexpr)
                     ^ " = " ^ (show_aexpr right_aexpr)))

and check_if_stmt (func: func) (if_stmt: if_stmt) (env: env) =
  (* Typecheck the condition and make sure its yields a boolean type. *)
  let cond = check_expr if_stmt.cond env in
  if not (phys_equal cond.typ TyBool)
  then raise (Error ("if stmt condition is not boolean: " ^
                     (show_if_stmt if_stmt)))
  else
    (* Typecheck the body. *)
    let then_br = check_stmt func if_stmt.then_br env in
    let else_br = check_stmt func if_stmt.else_br env in
    ({ cond; then_br; else_br }: tif_stmt)

and check_block_stmt (func: func) (block_stmt: block_stmt) (env: env) =
  let tblock_body = List.map block_stmt.block_body
      ~f: (fun stmt -> check_stmt func stmt env)
  in
  ({ tblock_body }: tblock_stmt)

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
  (* Save variables to be restored at the end. *)
  let saved_vars = env.vars in
  (* Make sure that there are no overlapping function names. *)
  if Map.mem env.funcs func.name
  then raise (Error ("duplicate function name -- "  ^ func.name))
  else
    (** Add name to function map. *)
    let funcs = Map.set env.funcs ~key: func.name ~data: func in
    (** Add each variable to the environment. *)
    let env, var_decls = List.fold_map (func.locals)
        ~init: env
        ~f: (fun env b -> check_bind b env)
    in
    let env, params = List.fold_map (func.params)
        ~init: env
        ~f: (fun env b -> check_bind b env)
    in
    (* Type each statement. *)
    let typed_stmts = List.map func.body ~f: (fun s -> check_stmt func s env) in
    { vars = saved_vars; funcs } ,
    ({ret_typ = func.ret_typ; name = func.name; params; locals = var_decls; body = typed_stmts}: tfunc)

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
