type range = Lexing.position * Lexing.position

val pp_range : Format.formatter -> range -> unit

val show_range : range -> string

type error =
  | Lexer_error of range
  | Parser_error of range
  | Type_error of range

val pp_error : char Gen.t -> Format.formatter -> error -> unit

val range : error -> range

val message : error -> string
