type pprogram = pdefinition list

and pdefinition =
  | PFunction of
      { name : string
      ; parameters : (string * ptype) list
      ; return_type : ptype
      ; body : pexpression
      ; range : Eon_report.range
      }
  | PType_alias of
      { name : string
      ; value : ptype
      ; range : Eon_report.range
      }
  | PType_record of
      { name : string
      ; fields : (string * ptype) list
      ; range : Eon_report.range
      }

and ptype =
  | PNamed_type of
      { name : string
      ; range : Eon_report.range
      }
  | PPointer_type of
      { underlying_type : ptype
      ; range : Eon_report.range
      }
  | PArray_type of
      { element_type : ptype
      ; range : Eon_report.range
      }
  | PFunction_type of
      { parameters : ptype list
      ; return_type : ptype
      ; range : Eon_report.range
      }

and pexpression =
  | PIdentifier of
      { name : string
      ; range : Eon_report.range
      }
  | PUnit of { range : Eon_report.range }
  | PBoolean of
      { value : bool
      ; range : Eon_report.range
      }
  | PInteger of
      { value : int64
      ; range : Eon_report.range
      }
  | PFloat of
      { value : float
      ; range : Eon_report.range
      }
  | PString of
      { value : string
      ; range : Eon_report.range
      }
  | PArray of
      { elements : pexpression list
      ; range : Eon_report.range
      }
  | PRecord of
      { name : string
      ; fields : (string * pexpression) list
      ; range : Eon_report.range
      }
  | PIndex of
      { expression : pexpression
      ; index : pexpression
      ; range : Eon_report.range
      }
  | PAccess of
      { expression : pexpression
      ; field : string
      ; range : Eon_report.range
      }
  | PAssign of
      { target : pexpression
      ; source : pexpression
      ; range : Eon_report.range
      }
  | PApply of
      { func : pexpression
      ; arguments : pexpression list
      ; range : Eon_report.range
      }
  | PUnary_operator of
      { operator : unary_operator
      ; expression : pexpression
      ; range : Eon_report.range
      }
  | PBinary_operator of
      { left : pexpression
      ; operator : binary_operator
      ; right : pexpression
      ; range : Eon_report.range
      }
  | PBlock of
      { body : pblock
      ; range : Eon_report.range
      }
  | PLet of
      { name : string
      ; value_type : ptype option
      ; value : pexpression
      ; range : Eon_report.range
      }
  | PIf of
      { condition : pexpression
      ; true_branch : pexpression
      ; false_branch : pexpression
      ; range : Eon_report.range
      }
  | PClosure of
      { parameters : (string * ptype) list
      ; return_type : ptype option
      ; body : pexpression
      ; range : Eon_report.range
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

val pdefinition_range : pdefinition -> Eon_report.range

val ptype_range : ptype -> Eon_report.range

val pexpression_range : pexpression -> Eon_report.range
