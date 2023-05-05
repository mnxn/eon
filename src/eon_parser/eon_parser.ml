include Ast

type token = [%import: Token.token] [@@deriving show { with_path = false }]

type position =
  { line : int
  ; column : int
  ; offset : int
  }

type error_range =
  { file_name : string
  ; start_pos : position
  ; end_pos : position
  }

let lex = Lexer.lex

let parse lexer lexbuf =
  let lexer = Sedlexing.with_tokenizer lexer lexbuf in
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  try Ok (parse lexer) with
  | Lexer.Error ->
    let start, _ = Sedlexing.lexing_positions lexbuf in
    let file_name = start.pos_fname in
    let start_pos =
      { line = start.pos_lnum
      ; column = start.pos_cnum - start.pos_bol + 1
      ; offset = start.pos_cnum
      }
    in
    let end_pos =
      { start_pos with column = start_pos.column + 1; offset = start_pos.offset + 1 }
    in
    Error { file_name; start_pos; end_pos }
  | Parser.Error ->
    let start, curr = Sedlexing.lexing_positions lexbuf in
    let file_name = start.pos_fname in
    let start_pos =
      { line = start.pos_lnum
      ; column = start.pos_cnum - start.pos_bol + 1
      ; offset = start.pos_cnum
      }
    in
    let end_pos =
      { line = curr.pos_lnum
      ; column = curr.pos_cnum - start.pos_bol + 1
      ; offset = curr.pos_cnum
      }
    in
    Error { file_name; start_pos; end_pos }
