{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white   { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }

  | '('       { LPAREN }
  | ')'       { RPAREN }
  | '{'       { LBRACE }
  | '}'       { RBRACE }
  | ';'       { SEMICOLON }
  | '='       { ASSIGN }

  | '+'       { ADD }
  | '-'       { SUB }
  | '*'       { MUL }
  | '/'       { DIV }
  | "=="      { EQ }
  | "!="      { NEQ }
  | '<'       { LT }
  | "<="      { LEQ }
  | '>'       { GT }
  | ">="      { GEQ }

  (* Values. *)
  | int       { INT (int_of_string (Lexing.lexeme lexbuf)) }

  (* Reserved Words. *)
  | "return"  { RETURN }
  | "true"    { BOOL true }
  | "false"   { BOOL false }
  | "if"      { IF }
  | "else"    { ELSE }

  (* Types. *)
  | "int"     { TYP TyInt }
  | "bool"    { TYP TyBool }

  | ident     { IDENT (Lexing.lexeme lexbuf) }

  | eof       { EOF }
  | _         { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
