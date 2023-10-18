type range = Lexing.position * Lexing.position

val pp_range : Format.formatter -> range -> unit

val show_range : range -> string

type printable

val box_pp : 'a -> (Format.formatter -> 'a -> unit) -> printable

val pp_printable : Format.formatter -> printable -> unit

type type_error =
  | Type_not_in_scope of string
  | Value_not_in_scope of string
  | Record_missing of string
  | Type_mismatch of
      { expected : printable
      ; actual : printable
      }
  | Not_numeric of printable
  | Not_pointer of printable
  | Not_array of printable
  | Empty_array
  | Not_record of printable
  | No_field of
      { actual : printable
      ; field : string
      }
  | Initializer_no_field of
      { expected : printable
      ; field : string
      }
  | Initializer_missing_field_value of
      { expected : printable
      ; field : string
      }
  | Initializer_duplicate_field of
      { expected : printable
      ; field : string
      }
  | Not_function of printable
  | Argument_count_mismatch of
      { expected : int
      ; actual : int
      }
  | If_branch_mismatch of
      { true_branch : printable
      ; false_branch : printable
      }

type runtime_error =
  | Undefined_value of string
  | Zero_division
  | Array_out_of_bounds of { index : int }
  | Value_shape_mismatch of
      { expected : printable
      ; actual : printable
      }
  | Value_argument_count_mismatch of
      { expected : int
      ; actual : int
      }

type error =
  | Lexer_error of range
  | Parser_error of range
  | Type_error of
      { type_error : type_error
      ; range : range
      }
  | Runtime_error of
      { runtime_error : runtime_error
      ; range : range option
      }

val pp_error : char Gen.t -> Format.formatter -> error -> unit

val range : error -> range option
