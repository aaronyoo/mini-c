%{
open Ast

let fst (a, _) = a
let snd (_, b) = b
let thd (_, _, c) = c
%}

%token <Ast.typ> TYP
%token <string> IDENT
%token <int> INT

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEMICOLON
%token ASSIGN

%token ADD
%token SUB
%token MUL
%token DIV

%token RETURN

%token EOF

%left ADD SUB
%left MUL DIV

%start <Ast.program> program;

%%

program:
  d = decls; EOF { {var_decls = fst d; func_decls = snd d;} }

decls:
  | { ([], []) }
  | d = decls; v = vdecl  { (v :: fst d, snd d) }
  | d = decls; f = fdecl  { (fst d, f :: snd d) }

fdecl:
  t = TYP; i = IDENT; LPAREN; RPAREN; LBRACE; vl = list(vdecl); sl = list(stmt); RBRACE;
  { {ret_typ = t; name = i; locals = vl; body = sl}: func }

vdecl:
  | t = TYP; i = IDENT; SEMICOLON
    { { bind_type = t; bind_name = i; initial_value = None }: bind }
  | t = TYP; i = IDENT; ASSIGN; e = expr; SEMICOLON
    { { bind_type = t; bind_name = i; initial_value = Some e }: bind }

stmt:
  | RETURN; e = expr; SEMICOLON;  { Return e }
  | e1 = expr; ASSIGN; e2 = expr; SEMICOLON
    { Assign ({ assign_left = e1; assign_right = e2 }: assign_stmt) }

expr:
  | i = INT  { IntLit i }
  | id = IDENT  { Ident ({ literal = id}: ident_expr) }
  | e1 = expr; ADD; e2 = expr;
    { Binop ({binop_type = Add; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; SUB; e2 = expr;
    { Binop ({binop_type = Sub; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; MUL; e2 = expr;
    { Binop ({binop_type = Mul; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; DIV; e2 = expr;
    { Binop ({binop_type = Div; binop_left = e1; binop_right = e2} :binop_expr) }
