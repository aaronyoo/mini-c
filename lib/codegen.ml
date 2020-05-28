open Llvm
open Base

exception Error of string

let context = global_context()
let the_module = create_module context "minicc"
let builder = builder context
let i64_type = i64_type context

let gen_expr (expr: Tast.aexpr) =
  match expr.expr with
  | Tast.TIntLit i -> const_int i64_type i

let gen_stmt (stmt: Tast.tstmt) =
  match stmt with
  | Tast.TReturn expr -> build_ret (gen_expr expr) builder

let gen_func_decl (tfunc: Tast.tfunc) =
  (* For now, there are no arguments and only and integer return type. *)
  let ft = match tfunc.ret_typ with
    | Ast.TyInt -> function_type i64_type (Array.create ~len: 0 i64_type)
  in
  (* Declare the function in the module. *)
  let the_function = declare_function tfunc.name ft the_module in

  (* Declare and entry basic block. *)
  let bb = append_block context "entry" the_function in
  let () = position_at_end bb builder in

  (* Generate the body of the block. *)
  List.iter tfunc.body ~f: (fun tstmt -> ignore(gen_stmt tstmt));
  Llvm_analysis.assert_valid_function the_function;
  the_function

let gen_program (tprog: Tast.tprog) =
  List.iter tprog.func_decls ~f: (fun tfunc -> ignore(gen_func_decl tfunc))