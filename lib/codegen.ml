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
let i1_type = i1_type context

let trans_asttyp (typ: Typ.t): lltype =
  match typ with
  | TyInt -> i64_type
  | TyBool -> i1_type

let gen_lval (lval: TLval.t) (env: env): llvalue * llvalue =
  (* Returns the result of a load instruction as well as the actual pointer. In
     cases that do not need to write to the varaible (and thus, do not need the
     pointer value) it is the callers responsibility to discard it. *)
  match lval with
  | TIdent id ->
    match Map.find env.var_names id with
    | None -> raise (Error ("identifier used before definition: " ^ id))
    | Some v ->
      (* Load the value of the identifier *)
      (build_load v "" builder), v

let rec gen_expr (expr: AExpr.t) (env: env) =
  match expr.expr with
  | TIntLit i -> const_int i64_type i
  | TBoolLit b -> (
      match b with
      | true -> const_int i1_type 1
      | false -> const_int i1_type 0
    )
  (* Discard the lval pointer because we are not writing. *)
  | TLval l -> fst (gen_lval l env)
  | TBinop (lhs, op, rhs) -> (
      let left_type = lhs.typ in
      let right_type = rhs.typ in
      let left = gen_expr lhs env in
      let right = gen_expr rhs env in
      match (left_type, right_type) with
      | (TyInt, TyInt) ->
        (* Operations defined over pairs of integers. *)
        (match op with
         | Add -> build_add left right "" builder
         | Sub -> build_sub left right "" builder
         | Mul -> build_mul left right "" builder
         | Div -> build_sdiv left right "" builder
         | Eq -> build_icmp Icmp.Eq left right "" builder
         | Neq -> build_icmp Icmp.Ne left right "" builder
         | Greater -> build_icmp Icmp.Sgt left right "" builder
         | Geq -> build_icmp Icmp.Sge left right "" builder
         | Less -> build_icmp Icmp.Slt left right "" builder
         | Leq -> build_icmp Icmp.Sle left right "" builder
        )
      | (TyBool, TyBool) ->
        (* Operations defined over pairs of bools. *)
        (match op with
         | Eq -> build_icmp Icmp.Eq left right "" builder
         | Neq -> build_icmp Icmp.Ne left right "" builder
         | Greater -> build_icmp Icmp.Sgt left right "" builder
         | Geq -> build_icmp Icmp.Sge left right "" builder
         | Less -> build_icmp Icmp.Slt left right "" builder
         | Leq -> build_icmp Icmp.Sle left right "" builder
         | _ -> failwith ("unimplemented gen_binop: \n" ^
                          (TExpr.show expr.expr))
        )
      | _ -> failwith ("unimplemented gen_binop: \n" ^
                       (TExpr.show expr.expr))
    )
  | TCall (callee, args) ->
    let evaluated_args  = List.map args ~f: (fun targ -> gen_expr targ env) in
    let callee = match lookup_function callee the_module with
      | Some c -> c
      | None -> raise (Error ("unknown function referenced: " ^ callee))
    in
    build_call callee (Array.of_list evaluated_args) "" builder

and gen_stmt (stmt: TStmt.t) (env: env): env * llvalue =
  match stmt with
  | TReturn expr -> env, build_ret (gen_expr expr env) builder
  | TAssign (lhs, rhs) ->
    let lval = match lhs.expr with
      | TLval lval -> lval
      |  _ -> failwith "unreachable"
    in
    let _, left_ptr = gen_lval lval env in
    let right = gen_expr rhs env in
    env, build_store right left_ptr builder
  | TIf (cond, t, f) ->
    (* Emit condition check into current block. *)
    let cond = gen_expr cond env in
    let one = const_int i1_type 1 in
    let cond_val = build_icmp Icmp.Eq cond one "ifcond" builder in

    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    let then_bb = append_block context "then" the_function in
    let else_bb = append_block context "else" the_function in

    (* Emit then block. *)
    position_at_end then_bb builder;
    let then_val = gen_stmt t env in

    (* Generating the then block can dirty the builder. *)
    let new_then_bb = insertion_block builder in

    (* Emit else block. *)
    position_at_end else_bb builder;
    let else_val = gen_stmt f env in
    let new_else_bb = insertion_block builder in

    (* Emit merge block. *)
    let merge_bb = append_block context "ifcont" the_function in
    position_at_end merge_bb builder;

    (* Return to the start block to add conditional branch. *)
    position_at_end start_bb builder;
    ignore(build_cond_br cond_val then_bb else_bb builder);

    (* Set unconditional branch at end of 'then' block. *)
    position_at_end new_then_bb builder;
    ignore(build_br merge_bb builder);

    (* Set unconditional branch at end of 'else' block. *)
    position_at_end new_else_bb builder;
    ignore(build_br merge_bb builder);

    position_at_end merge_bb builder;

    ignore(then_val);
    (* Ignore the environment changes in the branches. *)
    env, snd (else_val)
  | TBlock body ->
    let env, gen_body = List.fold_map body ~init: env ~f: (fun env tstmt -> gen_stmt tstmt env) in
    (* Get the llvalue for the last statement generated. *)
    let value = match List.hd (List.rev gen_body) with
      | Some e -> e
      | None -> raise (Error "Illegal empty block")
    in
    env, value
  | TFor (init, cond, inc, body) ->
    (* Emit the initialization into the current basic block. Since the
       initialization block is only run once. *)
    let env, _  = gen_stmt init env in

    (* Create all the needed basic blocks. *)
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    let cond_bb = append_block context "forcond" the_function in
    let body_bb = append_block context "forbody" the_function in
    let end_bb = append_block context "" the_function in

    (* Create the condition block. Insert an unconditional jump to the condition
       check. Emit the condition check code. Compare the value to true and save
       the result as we will use it to conditionally jump later. *)
    ignore(build_br cond_bb builder);
    position_at_end cond_bb builder;
    let cond = gen_expr cond env in
    let one = const_int i1_type 1 in
    let cond_val = build_icmp Icmp.Eq cond one "cond" builder in
    ignore(build_cond_br cond_val body_bb end_bb builder);

    (* Emit the body and the increment in one basic block and build an
       unconditional jump back to the condition. *)
    position_at_end body_bb builder;
    ignore(gen_stmt body env);
    ignore(gen_stmt inc env);
    ignore(build_br cond_bb builder);

    position_at_end end_bb builder;

    env, cond_val
  | TBind tbind ->
    (* Add a new name to the environment. *)
    let var = build_alloca (trans_asttyp tbind.typ) tbind.name builder in
    let vars = Map.set env.var_names ~key:tbind.name ~data:var in
    { var_names = vars; }, var

(** Generates an Array of parameter types. *)
let gen_param_types (params: TBind.t list) =
  let param_types = List.map params ~f: (fun tbind ->
      trans_asttyp tbind.typ)
  in
  Array.of_list param_types

(** Generates a prototype for a function. This can be retrieved later from the
    module when the actual function is populated. Here, its particularly used to
    predeclare all the functions so that there is not need for forward
    declarations. *)
let gen_proto (tfunc: TFunc.t) =
  let param_typ = gen_param_types tfunc.params in
  let ret_typ = trans_asttyp tfunc.typ in
  let ft = function_type ret_typ param_typ in
  ignore(declare_function tfunc.name ft the_module)

let gen_func_decl (tfunc: TFunc.t) (env: env) =
  (* Get the function from the module table. It should already exist. *)
  let the_function = match lookup_function tfunc.name the_module with
    | Some f -> f
    | None -> raise (Error ("Function not declared: " ^ tfunc.name))
  in

  (* Declare and entry basic block. *)
  let bb = append_block context "entry" the_function in
  let () = position_at_end bb builder in

  (* Introduce the arguments into the environment. We also push the arguments to
     the stack. This is essentially the equivalent of spilling everything to the
     stack but the LLVM optimizing backend allows us not to worry about allows
     LLVM to promote to registers. *)
  let env =
    match
      List.fold2 (Array.to_list (params the_function)) (tfunc.params)
        ~init: env
        ~f:(fun env ll_p ty_p ->
            (* Create space on the stack and move the argument into
                that space. Then use this value as the argument for the
                rest of the function. *)
            let p = build_alloca (trans_asttyp ty_p.typ) (ty_p.name) builder in
            ignore(build_store ll_p p builder);
            set_value_name ty_p.name p;
            let var_names = Map.set env.var_names ~key: ty_p.name ~data: p in
            { var_names }
          )
    with
    | Ok env -> env
    | Unequal_lengths -> failwith "unreachable"
  in

  (* Generate the body of the function. *)
  ignore(List.fold tfunc.body ~init:env ~f:(fun env tstmt -> fst (gen_stmt tstmt env)));
  Llvm_analysis.assert_valid_function the_function;
  the_function

let gen_program (tprog: TProgram.t) =
  (* The environment to each function should be empty. *)
  let env = empty_env in
  (* In order to remove the need for forward declarations, we can automatically
     predeclare each local function. This puts all functions in the module table
     to be looked up later in something like a call instruction. *)
  List.iter tprog.func_decls ~f: (fun tfunc -> gen_proto tfunc);
  (* Create all the function declarations. *)
  List.iter tprog.func_decls ~f: (fun tfunc -> ignore(gen_func_decl tfunc env))
