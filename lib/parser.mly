%{
open Ast

let fst (a, _) = a
let snd (_, b) = b
let thd (_, _, c) = c
%}

%token <Ast.Typ.t> TYP
%token <string> IDENT
%token <int> INT
%token <bool> BOOL

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEMICOLON
%token COMMA
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
%token FOR

%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left EQ NEQ
%left LT GT LEQ GEQ
%left ADD SUB
%left MUL DIV

%start <Ast.Program.t> program;

%%

program:
  d = decls; EOF { {func_decls = d;} }

decls:
  | { [] }
  | d = decls; f = fdecl  { f :: d }

fdecl:
  typ = TYP; name = IDENT; LPAREN; params = separated_list(COMMA, bind);
  RPAREN; LBRACE; body = list(stmt); RBRACE;
  { {typ; name; params; body;}: Func.t }

bind:
  | typ = TYP; name = IDENT;
  { {typ; name}: Bind.t }

expr_stmt:
  | RETURN; e = expr;  { Stmt.Return e }
  | b = bind; { Stmt.Bind b }
  | e1 = expr; ASSIGN; e2 = expr; { Stmt.Assign (e1, e2) }

stmt:
  | es = expr_stmt SEMICOLON;  { es }
  | LBRACE; l = list(stmt); RBRACE;
    { Stmt.Block l }
  | IF; LPAREN; cond = expr; RPAREN; then_br = stmt; %prec NOELSE
    { Stmt.If (cond, then_br, Stmt.Block []) }
  | IF; LPAREN; cond = expr; RPAREN; then_br = stmt; ELSE; else_br = stmt
    { Stmt.If (cond, then_br, else_br) }
  | FOR; LPAREN; for_init = expr_stmt; SEMICOLON; for_cond = expr; SEMICOLON;
    for_inc = expr_stmt; RPAREN; for_body = stmt;
    { Stmt.For (for_init, for_cond, for_inc, for_body) }

expr:
  | i = INT                       { Expr.IntLit i }
  | b = BOOL                      { Expr.BoolLit b }
  | id = IDENT                    { Expr.Ident id }
  | e1 = expr; ADD; e2 = expr;    { Expr.Binop (e1, Op.Add, e2) }
  | e1 = expr; SUB; e2 = expr;    { Expr.Binop (e1, Op.Sub, e2) }
  | e1 = expr; MUL; e2 = expr;    { Expr.Binop (e1, Op.Mul, e2) }
  | e1 = expr; DIV; e2 = expr;    { Expr.Binop (e1, Op.Div, e2) }
  | e1 = expr; EQ; e2 = expr;     { Expr.Binop (e1, Op.Eq, e2) }
  | e1 = expr; NEQ; e2 = expr;    { Expr.Binop (e1, Op.Neq, e2) }
  | e1 = expr; LT; e2 = expr;     { Expr.Binop (e1, Op.Less, e2) }
  | e1 = expr; GT; e2 = expr;     { Expr.Binop (e1, Op.Greater, e2) }
  | e1 = expr; LEQ; e2 = expr;    { Expr.Binop (e1, Op.Leq, e2) }
  | e1 = expr; GEQ; e2 = expr;    { Expr.Binop (e1, Op.Geq, e2) }
  | callee = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN
    { Expr.Call (callee, args)}
