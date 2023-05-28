type range =
  { start_pos : Lexing.position
  ; end_pos : Lexing.position
  }

val pp_range : Format.formatter -> range -> unit

val show_range : range -> string

val pos : Lexing.position -> range

val loc : Lexing.position * Lexing.position -> range

type error =
  | Lexer_error of range
  | Parser_error of range
  | Type_error

val pp_error : in_channel -> Format.formatter -> error -> unit

val range : error -> range

val message : error -> string
