include Ast

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
    let start, _ = Sedlexing.lexing_positions lexbuf in
    let file_name = start.pos_fname in
    let start_pos =
      { Eon_report.line = start.pos_lnum
      ; column = start.pos_cnum - start.pos_bol + 1
      ; offset = start.pos_cnum
      }
    in
    let end_pos =
      { start_pos with column = start_pos.column + 1; offset = start_pos.offset + 1 }
    in
    let range = { Eon_report.file_name; start_pos; end_pos } in
    Error (Eon_report.Lexer_error range)
  | Parser.Error ->
    let start, curr = Sedlexing.lexing_positions lexbuf in
    let file_name = start.pos_fname in
    let start_pos =
      { Eon_report.line = start.pos_lnum
      ; column = start.pos_cnum - start.pos_bol + 1
      ; offset = start.pos_cnum
      }
    in
    let end_pos =
      { Eon_report.line = curr.pos_lnum
      ; column = curr.pos_cnum - start.pos_bol + 1
      ; offset = curr.pos_cnum
      }
    in
    let range = { Eon_report.file_name; start_pos; end_pos } in
    Error (Eon_report.Parser_error range)
