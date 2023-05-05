open Sedlexing
open Token

exception Error

let digit = [%sedlex.regexp? '0' .. '9']

let number = [%sedlex.regexp? digit, Star (digit | '_')]

let float = [%sedlex.regexp? number, Opt ('.', number)]

let string = [%sedlex.regexp? '"', Star (Compl '"'), '"']

let comment = [%sedlex.regexp? "//", Star (Compl '\n')]

let id_char = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_']

let identifier = [%sedlex.regexp? id_char, Star (id_char | digit)]

let rec lex lexbuf =
  match%sedlex lexbuf with
  | white_space -> lex lexbuf
  | comment -> lex lexbuf
  | "true" -> BOOLEAN true
  | "false" -> BOOLEAN false
  | "let" -> LET
  | "func" -> FUNC
  | "type" -> TYPE
  | "if" -> IF
  | "then" -> THEN
  | "else" -> ELSE
  | ";" -> SEMICOLON
  | ":" -> COLON
  | "." -> DOT
  | "," -> COMMA
  | "&" -> AMPERSAND
  | "^" -> HAT
  | "!" -> NOT
  | "&&" -> AND
  | "||" -> OR
  | "=" -> EQUAL
  | "==" -> EQUAL_EQUAL
  | "!=" -> NOT_EQUAL
  | "<" -> LESS
  | "<=" -> LESS_EQUAL
  | ">" -> GREATER
  | ">=" -> GREATER_EQUAL
  | "+" -> PLUS
  | "-" -> DASH
  | "*" -> ASTERISK
  | "/" -> SLASH
  | "%" -> PERCENT
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "[" -> LBRACKET
  | "]" -> RBRACKET
  | "{" -> LCURLY
  | "}" -> RCURLY
  | "<-" -> LARROW
  | "->" -> RARROW
  | eof -> EOF
  | number -> INTEGER (Int64.of_string @@ Utf8.lexeme lexbuf)
  | float -> FLOAT (Float.of_string @@ Utf8.lexeme lexbuf)
  | string -> STRING (Utf8.lexeme lexbuf)
  | identifier -> IDENTIFIER (Utf8.lexeme lexbuf)
  | _ -> raise Error
