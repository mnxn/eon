module Parsetree = Parsetree

type token =
  | TYPE
  | THEN
  | STRING of string
  | SLASH
  | SEMICOLON
  | RPAREN
  | RCURLY
  | RBRACKET
  | RARROW
  | PLUS
  | PERCENT
  | OR
  | NOT_EQUAL
  | NOT
  | LPAREN
  | LET
  | LESS_EQUAL
  | LESS
  | LCURLY
  | LBRACKET
  | LARROW
  | INTEGER of int64
  | IF
  | IDENTIFIER of string
  | HAT
  | GREATER_EQUAL
  | GREATER
  | FUNC
  | FLOAT of float
  | EQUAL_EQUAL
  | EQUAL
  | EOF
  | ELSE
  | DOT
  | DASH
  | COMMA
  | COLON
  | BOOLEAN of bool
  | ASTERISK
  | AND
  | AMPERSAND

val pp_token : Format.formatter -> token -> unit

val show_token : token -> string

val lex : Sedlexing.lexbuf -> token

val lex_all : Sedlexing.lexbuf -> (token list, Eon_report.error) result

val parse_program
  :  (Sedlexing.lexbuf -> token)
  -> Sedlexing.lexbuf
  -> (Parsetree.pprogram, Eon_report.error) result

val parse_expression
  :  (Sedlexing.lexbuf -> token)
  -> Sedlexing.lexbuf
  -> (Parsetree.pexpression, Eon_report.error) result
