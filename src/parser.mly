%{
open Ast
%}

%token <Ast.typ> TYP
%token <string> IDENT
%token <int> INT

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token SEMICOLON

%token RETURN

%token EOF

%start <Ast.program> program;

%%

program:
  l = list(fdecl); EOF { {func_decls = l} }

fdecl:
  t = TYP; i = IDENT; LPAREN; RPAREN; LBRACE; sl = list(stmt); RBRACE;
  { {ret_typ = t; name = i; body = sl}: Ast.func_decl }

stmt:
  RETURN; e = expr; SEMICOLON;  { Return e }

expr:
  i = INT  { IntLit i }