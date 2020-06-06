open Llvm
open Base
open Ast
open Tast

exception Error of string

type env = {
  var_names: llvalue Map.M(String).t
}

let empty_env: env = {
  var_names = Map.empty(module String)
}

let context = global_context()
let the_module = create_module context "minicc"
let builder = builder context
let i64_type = i64_type context

let trans_asttyp (typ: typ): lltype =
  match typ with
  | TyInt -> i64_type

let gen_lval (lval: tlval) (env: env) =
  (* Returns the result of a load instruction as well as the actual pointer. In
     cases that do not need to write to the varaible (and thus, do not need the
     pointer value) it is the callers responsibility to discard it. *)
  match lval with
  | TIdent id ->
    match Map.find env.var_names id.literal with
    | None -> raise (Error ("identifier used before definition: " ^ id.literal))
    | Some v ->
      (* Load the value of the identifier *)
      (build_load v "" builder), v

let rec gen_expr (expr: aexpr) (env: env) =
  match expr.expr with
  | TIntLit i -> const_int i64_type i
  (* Discard the lval pointer because we are not writing. *)
  | TLval l -> fst (gen_lval l env)
  | TBinop b -> gen_binop b env

and gen_binop (bexpr: tbinop_expr) (env: env) =
  let left = gen_expr bexpr.tbinop_left env in
  let right = gen_expr bexpr.tbinop_right env in
  match bexpr.binop_type with
  (* Only deal with integer types for now. *)
  | Add -> build_add left right "" builder
  | Sub -> build_sub left right "" builder
  | Mul -> build_mul left right "" builder
  | Div -> build_sdiv left right "" builder

let gen_assign_stmt (assign_stmt: tassign_stmt) (env: env) =
  let lval = match assign_stmt.tassign_left.expr with
    | TLval lval -> lval
    |  _ -> failwith "unreachable"
  in
  let _, left_ptr = gen_lval lval env in
  let right = gen_expr assign_stmt.tassign_right env in
  build_store right left_ptr builder

let gen_stmt (stmt: tstmt) (env: env) =
  match stmt with
  | TReturn expr -> build_ret (gen_expr expr env) builder
  | TAssign assign_stmt -> gen_assign_stmt assign_stmt env

let gen_func_decl (tfunc: tfunc) (env: env) =
  (* For now, there are no arguments and only and integer return type. *)
  let ft = match tfunc.ret_typ with
    | TyInt -> function_type i64_type (Array.create ~len: 0 i64_type)
  in
  (* Declare the function in the module. *)
  let the_function = declare_function tfunc.name ft the_module in

  (* Declare and entry basic block. *)
  let bb = append_block context "entry" the_function in
  let () = position_at_end bb builder in

  (* Generate the local variables of the function. *)
  let env = List.fold tfunc.locals ~init: env ~f: (fun env bind ->
      let typ = trans_asttyp bind.bind_type in
      let var = build_alloca typ (bind.bind_name) builder in
      let _ = match bind.initial_value with
        | None -> ()
        (* If there is an initial value then store it. *)
        | Some e -> ignore(build_store (gen_expr e env) var builder)
      in
      let var_names = Map.set env.var_names ~key: bind.bind_name ~data: var in
      { var_names })
  in

  (* Generate the body of the function. *)
  List.iter tfunc.body ~f: (fun tstmt -> ignore(gen_stmt tstmt env));
  Llvm_analysis.assert_valid_function the_function;
  the_function

let gen_program (tprog: tprog) =
  (* Declare all of the global variables *)
  let global_vars = Map.empty(module String) in
  let global_vars = List.fold tprog.var_decls
      ~init: global_vars
      ~f: (fun globals decl ->
          let llval = declare_global (trans_asttyp decl.bind_type) decl.bind_name the_module in
          (* If there is an initial value then set an initializer. *)
          let _ = match decl.initial_value with
            | Some initial -> set_initializer (gen_expr initial empty_env) llval
            | None -> ()
          in
          match Map.add globals ~key: decl.bind_name ~data: llval with
          | `Ok m -> m
          | `Duplicate -> failwith ("duplicate global name -- " ^ decl.bind_name))
  in
  (* The environment to each function should include all globals *)
  let env = { var_names = global_vars } in
  List.iter tprog.func_decls ~f: (fun tfunc -> ignore(gen_func_decl tfunc env))
