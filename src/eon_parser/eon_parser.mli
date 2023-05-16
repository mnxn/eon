type program = definition list

and definition =
  | Function of
      { name : string
      ; parameters : (string * typ) list
      ; return_type : typ
      ; body : expression
      }
  | Type_alias of
      { name : string
      ; typ : typ
      }
  | Type_record of
      { name : string
      ; fields : (string * typ) list
      }

and typ =
  | Named_type of string
  | Pointer_type of typ
  | Slice_type of typ
  | Array_type of
      { element_type : typ
      ; length : int64
      }
  | Function_type of
      { parameters : typ list
      ; return_type : typ
      }

and expression =
  | Identifier of string
  | Unit
  | Boolean of bool
  | Integer of int64
  | Float of float
  | String of string
  | Array of expression list
  | Record of
      { name : string
      ; fields : (string * expression) list
      }
  | Index of
      { expression : expression
      ; index : expression
      }
  | Access of
      { expression : expression
      ; field : string
      }
  | Assign of
      { target : expression
      ; source : expression
      }
  | Apply of
      { func : expression
      ; arguments : expression list
      }
  | Unary_operator of
      { operator : unary_operator
      ; expression : expression
      }
  | Binary_operator of
      { left : expression
      ; operator : binary_operator
      ; right : expression
      }
  | Block of block
  | Let of
      { name : string
      ; typ : typ option
      ; value : expression
      }
  | If of
      { condition : expression
      ; true_branch : expression
      ; false_branch : expression
      }
  | Closure of
      { parameters : (string * typ) list
      ; return_type : typ option
      ; body : expression
      }

and block =
  { statements : expression list
  ; result : expression option
  }

and unary_operator =
  | Not
  | Negate
  | Address_of
  | Dereference

and binary_operator =
  | And
  | Or
  | Equal
  | Not_equal
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | Add
  | Subtract
  | Multiply
  | Divide
  | Remainder

val pp_program : Format.formatter -> program -> unit

val show_program : program -> string

val pp_definition : Format.formatter -> definition -> unit

val show_definition : definition -> string

val pp_typ : Format.formatter -> typ -> unit

val show_typ : typ -> string

val pp_expression : Format.formatter -> expression -> unit

val show_expression : expression -> string

val pp_block : Format.formatter -> block -> unit

val show_block : block -> string

val pp_unary_operator : Format.formatter -> unary_operator -> unit

val show_unary_operator : unary_operator -> string

val pp_binary_operator : Format.formatter -> binary_operator -> unit

val show_binary_operator : binary_operator -> string

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

val parse
  :  (Sedlexing.lexbuf -> token)
  -> Sedlexing.lexbuf
  -> (program, Eon_report.error) result
