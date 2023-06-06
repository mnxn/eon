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

let lex_all lexbuf =
  let rec go acc =
    match Lexer.lex lexbuf with
    | EOF -> List.rev acc
    | tok -> go (tok :: acc)
  in
  try Ok (go []) with
  | Lexer.Error ->
    let range = Sedlexing.lexing_positions lexbuf in
    Error (Eon_report.Lexer_error range)

let lex = Lexer.lex

let parse parser lexer lexbuf =
  let lexer = Sedlexing.with_tokenizer lexer lexbuf in
  let parse = MenhirLib.Convert.Simplified.traditional2revised parser in
  try Ok (parse lexer) with
  | Lexer.Error ->
    let range = Sedlexing.lexing_positions lexbuf in
    Error (Eon_report.Lexer_error range)
  | Parser.Error ->
    let range = Sedlexing.lexing_positions lexbuf in
    Error (Eon_report.Parser_error range)

let parse_program = parse Parser.program

let parse_expression = parse Parser.expression_eof
