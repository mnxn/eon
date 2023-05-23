type pprogram = pdefinition list

and pdefinition =
  | PFunction of
      { name : string
      ; parameters : (string * ptype) list
      ; return_type : ptype
      ; body : pexpression
      }
  | PType_alias of
      { name : string
      ; value : ptype
      }
  | PType_record of
      { name : string
      ; fields : (string * ptype) list
      }

and ptype =
  | PNamed_type of string
  | PPointer_type of ptype
  | PSlice_type of ptype
  | PArray_type of
      { element_type : ptype
      ; length : int64
      }
  | PFunction_type of
      { parameters : ptype list
      ; return_type : ptype
      }

and pexpression =
  | PIdentifier of string
  | PUnit
  | PBoolean of bool
  | PInteger of int64
  | PFloat of float
  | PString of string
  | PArray of pexpression list
  | PRecord of
      { name : string
      ; fields : (string * pexpression) list
      }
  | PIndex of
      { expression : pexpression
      ; index : pexpression
      }
  | PAccess of
      { expression : pexpression
      ; field : string
      }
  | PAssign of
      { target : pexpression
      ; source : pexpression
      }
  | PApply of
      { func : pexpression
      ; arguments : pexpression list
      }
  | PUnary_operator of
      { operator : unary_operator
      ; expression : pexpression
      }
  | PBinary_operator of
      { left : pexpression
      ; operator : binary_operator
      ; right : pexpression
      }
  | PBlock of pblock
  | PLet of
      { name : string
      ; value_type : ptype option
      ; value : pexpression
      }
  | PIf of
      { condition : pexpression
      ; true_branch : pexpression
      ; false_branch : pexpression
      }
  | PClosure of
      { parameters : (string * ptype) list
      ; return_type : ptype option
      ; body : pexpression
      }

and pblock =
  { statements : pexpression list
  ; result : pexpression option
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

val pp_pprogram : Format.formatter -> pprogram -> unit

val show_pprogram : pprogram -> string

val pp_pdefinition : Format.formatter -> pdefinition -> unit

val show_pdefinition : pdefinition -> string

val pp_ptype : Format.formatter -> ptype -> unit

val show_ptype : ptype -> string

val pp_pexpression : Format.formatter -> pexpression -> unit

val show_pexpression : pexpression -> string

val pp_pblock : Format.formatter -> pblock -> unit

val show_pblock : pblock -> string

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
  -> (pprogram, Eon_report.error) result
