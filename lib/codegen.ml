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

let trans_asttyp (typ: typ): lltype =
  match typ with
  | TyInt -> i64_type
  | TyBool -> i1_type

let gen_bool_lit (b: bool) =
  match b with
  | true -> const_int i1_type 1
  | false -> const_int i1_type 0

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
  | TBoolLit b -> gen_bool_lit b
  (* Discard the lval pointer because we are not writing. *)
  | TLval l -> fst (gen_lval l env)
  | TBinop b -> gen_binop b env
  | TCall c -> gen_call c env

and gen_binop (bexpr: tbinop_expr) (env: env) =
  let left = gen_expr bexpr.tbinop_left env in
  let right = gen_expr bexpr.tbinop_right env in
  let left_type = bexpr.tbinop_left.typ in
  let right_type = bexpr.tbinop_right.typ in
  match (left_type, right_type) with
  | (TyInt, TyInt) ->
    (* Operations defined over pairs of integers. *)
    (match bexpr.binop_type with
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
    (match bexpr.binop_type with
     | Eq -> build_icmp Icmp.Eq left right "" builder
     | Neq -> build_icmp Icmp.Ne left right "" builder
     | Greater -> build_icmp Icmp.Sgt left right "" builder
     | Geq -> build_icmp Icmp.Sge left right "" builder
     | Less -> build_icmp Icmp.Slt left right "" builder
     | Leq -> build_icmp Icmp.Sle left right "" builder
     | _ -> failwith ("unimplemented gen_binop: \n" ^
                      (show_tbinop_expr bexpr))
    )
  | _ -> failwith ("unimplemented gen_binop: \n" ^
                   (show_tbinop_expr bexpr))

and gen_call (tcall_expr: tcall_expr) (env: env) =
  let evaluated_args = List.map tcall_expr.targs ~f: (fun targ -> gen_expr targ env) in
  let callee = match lookup_function tcall_expr.tcallee the_module with
    | Some c -> c
    | None -> raise (Error ("unknown function referenced: " ^ tcall_expr.tcallee))
  in
  build_call callee (Array.of_list evaluated_args) "" builder

let gen_assign_stmt (assign_stmt: tassign_stmt) (env: env) =
  let lval = match assign_stmt.tassign_left.expr with
    | TLval lval -> lval
    |  _ -> failwith "unreachable"
  in
  let _, left_ptr = gen_lval lval env in
  let right = gen_expr assign_stmt.tassign_right env in
  build_store right left_ptr builder

let rec gen_if_stmt (if_stmt: tif_stmt) (env: env) =
  (* Emit condition check into current block. *)
  let cond = gen_expr if_stmt.cond env in
  let one = const_int i1_type 1 in
  let cond_val = build_icmp Icmp.Eq cond one "ifcond" builder in

  let start_bb = insertion_block builder in
  let the_function = block_parent start_bb in
  let then_bb = append_block context "then" the_function in
  let else_bb = append_block context "else" the_function in

  (* Emit then block. *)
  position_at_end then_bb builder;
  let then_val = gen_stmt if_stmt.then_br env in

  (* Generating the then block can dirty the builder. *)
  let new_then_bb = insertion_block builder in

  (* Emit else block. *)
  position_at_end else_bb builder;
  let else_val = gen_stmt if_stmt.else_br env in
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
  ignore(else_val);

and gen_block_stmt (block_stmt: tblock_stmt) (env: env) =
  List.iter block_stmt.tblock_body ~f: (fun tstmt -> gen_stmt tstmt env)

and gen_stmt (stmt: tstmt) (env: env) =
  (* Since these are all statements we can ignore any llvalues that are
     returned. *)
  match stmt with
  | TReturn expr -> ignore(build_ret (gen_expr expr env) builder)
  | TAssign assign_stmt -> ignore(gen_assign_stmt assign_stmt env)
  | TIf if_stmt -> gen_if_stmt if_stmt env
  | TBlock block_stmt -> gen_block_stmt block_stmt env

(** Generates an Array of parameter types. *)
let gen_param_types (params: tbind list) =
  let param_types = List.map params ~f: (fun tbind ->
      trans_asttyp tbind.bind_type)
  in
  Array.of_list param_types

(** Generates a prototype for a function. This can be retrieved later from the
    module when the actual function is populated. Here, its particularly used to
    predeclare all the functions so that there is not need for forward
    declarations. *)
let gen_proto (tfunc: tfunc) =
  let param_typ = gen_param_types tfunc.params in
  let ret_typ = trans_asttyp tfunc.ret_typ in
  let ft = function_type ret_typ param_typ in
  ignore(declare_function tfunc.name ft the_module)

let gen_func_decl (tfunc: tfunc) (env: env) =
  (* Get the function from the module table. It should already exist. *)
  let the_function = match lookup_function tfunc.name the_module with
    | Some f -> f
    | None -> raise (Error ("Function not declared: " ^ tfunc.name))
  in

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

  (* Introduce the arguments into the environment. We also push the arguments to
     the stack. This is essentially the equivalent of spilling everything to the
     stack but the LLVM optimizing backend allows us not to worry about allows
     LLVM to promote to registers. *)
  let env = match List.fold2 (Array.to_list (params the_function)) (tfunc.params)
                    ~init: env
                    ~f:(fun env ll_p ty_p ->
                        (* Create space on the stack and move the argument into
                           that space. Then use this value as the argument for the
                           rest of the function. *)
                        let p = build_alloca (trans_asttyp ty_p.bind_type) (ty_p.bind_name) builder in
                        ignore(build_store ll_p p builder);
                        set_value_name ty_p.bind_name p;
                        let var_names = Map.set env.var_names ~key: ty_p.bind_name ~data: p in
                        { var_names }
                      )
    with
    | Ok env -> env
    | Unequal_lengths -> failwith "unreachable"
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
  (* The environment to each function should include all globals. *)
  let env = { var_names = global_vars } in
  (* In order to remove the need for forward declarations, we can automatically
     predeclare each local function. This puts all functions in the module table
     to be looked up later in something like a call instruction. *)
  List.iter tprog.func_decls ~f: (fun tfunc -> gen_proto tfunc);
  (* Create all the function declarations. *)
  List.iter tprog.func_decls ~f: (fun tfunc -> ignore(gen_func_decl tfunc env))
