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

%token RETURN

%token EOF

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
  | e1 = expr; ASSIGN; e2 = expr; SEMICOLON  { Assign ({ left = e1; right = e2 }: assign_stmt) }

expr:
  | i = INT  { IntLit i }
  | id = IDENT  { Ident ({ literal = id}: ident_expr) }