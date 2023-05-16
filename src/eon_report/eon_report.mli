type position =
  { line : int
  ; column : int
  ; offset : int
  }

type range =
  { file_name : string
  ; start_pos : position
  ; end_pos : position
  }

type error =
  | Lexer_error of range
  | Parser_error of range

val pp_error : in_channel -> Format.formatter -> error -> unit

val range : error -> range

val message : error -> string
