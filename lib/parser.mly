%{
open Ast

let fst (a, _) = a
let snd (_, b) = b
let thd (_, _, c) = c
%}

%token <Ast.typ> TYP
%token <string> IDENT
%token <int> INT
%token <bool> BOOL

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
%token EQ
%token NEQ
%token LT
%token LEQ
%token GT
%token GEQ

%token RETURN
%token IF
%token ELSE

%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left EQ NEQ
%left LT GT LEQ GEQ
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
  | LBRACE; l = list(stmt); RBRACE;
    { Block ({block_body = l}: block_stmt) }
  | IF; LPAREN; cond = expr; RPAREN; then_br = stmt; %prec NOELSE
    { If ({ cond; then_br; else_br = Block ({ block_body = [] }: block_stmt) }: if_stmt) }
  | IF; LPAREN; cond = expr; RPAREN; then_br = stmt; ELSE; else_br = stmt
    { If ({ cond; then_br; else_br }: if_stmt) }


expr:
  | i = INT  { IntLit i }
  | b = BOOL  { BoolLit b }
  | id = IDENT  { Ident ({ literal = id}: ident_expr) }
  | e1 = expr; ADD; e2 = expr;
    { Binop ({binop_type = Add; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; SUB; e2 = expr;
    { Binop ({binop_type = Sub; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; MUL; e2 = expr;
    { Binop ({binop_type = Mul; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; DIV; e2 = expr;
    { Binop ({binop_type = Div; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; EQ; e2 = expr;
    { Binop ({binop_type = Eq; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; NEQ; e2 = expr;
    { Binop ({binop_type = Neq; binop_left = e1; binop_right = e2} :binop_expr)}
  | e1 = expr; LT; e2 = expr;
    {Binop ({binop_type = Less; binop_left = e1; binop_right = e2}:binop_expr)}
  | e1 = expr; GT; e2 = expr;
    {Binop ({binop_type = Greater; binop_left = e1; binop_right = e2}:binop_expr)}
  | e1 = expr; LEQ; e2 = expr;
    {Binop ({binop_type = Leq; binop_left = e1; binop_right = e2}:binop_expr)}
  | e1 = expr; GEQ; e2 = expr;
    {Binop ({binop_type = Geq; binop_left = e1; binop_right = e2}:binop_expr)}
