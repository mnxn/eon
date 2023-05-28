module Parsetree = Parsetree

type token = Token.token =
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
[@@deriving show { with_path = false }]

let lex = Lexer.lex

let parse lexer lexbuf =
  let lexer = Sedlexing.with_tokenizer lexer lexbuf in
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  try Ok (parse lexer) with
  | Lexer.Error ->
    let start_pos, _ = Sedlexing.lexing_positions lexbuf in
    let range = Eon_report.pos start_pos in
    Error (Eon_report.Lexer_error range)
  | Parser.Error ->
    let start_pos, end_pos = Sedlexing.lexing_positions lexbuf in
    let range = Eon_report.loc (start_pos, end_pos) in
    Error (Eon_report.Parser_error range)
