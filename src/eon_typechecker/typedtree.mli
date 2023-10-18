type cprogram = cdefinition list

and cdefinition =
  | CFunction of
      { name : string
      ; parameters : (string * ctype) list
      ; return_type : ctype
      ; body : cexpression
      ; range : Eon_report.range
      }
  | CType_alias of
      { name : string
      ; value : ctype
      ; range : Eon_report.range
      }
  | CType_record of
      { name : string
      ; record : crecord
      ; range : Eon_report.range
      }

and crecord = { fields : (string * ctype) list }

and ctype =
  | CPrimitive_type of string
  | CPointer_type of ctype
  | CArray_type of ctype
  | CFunction_type of
      { parameters : ctype list
      ; return_type : ctype
      }
  | CRecord_type of { name : string }

and cexpression =
  | CIdentifier of
      { name : string
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CUnit of
      { ctype : ctype
      ; range : Eon_report.range
      }
  | CBoolean of
      { value : bool
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CInteger of
      { value : int64
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CFloat of
      { value : float
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CString of
      { value : string
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CGroup of
      { expression : cexpression
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CArray of
      { elements : cexpression list
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CRecord of
      { name : string
      ; fields : (string * cexpression) list
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CIndex of
      { expression : cexpression
      ; index : cexpression
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CAccess of
      { expression : cexpression
      ; field : string
      ; field_index : int
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CAssign of
      { target : cexpression
      ; source : cexpression
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CApply of
      { func : cexpression
      ; arguments : cexpression list
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CUnary_operator of
      { operator : Eon_parser.Parsetree.unary_operator
      ; expression : cexpression
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CBinary_operator of
      { left : cexpression
      ; operator : Eon_parser.Parsetree.binary_operator
      ; right : cexpression
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CBlock of
      { block : cblock
      ; range : Eon_report.range
      }
  | CLet of
      { name : string
      ; value_type : ctype
      ; value : cexpression
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CIf of
      { condition : cexpression
      ; true_branch : cexpression
      ; false_branch : cexpression
      ; ctype : ctype
      ; range : Eon_report.range
      }
  | CClosure of
      { parameters : (string * ctype) list
      ; return_type : ctype
      ; body : cexpression
      ; ctype : ctype
      ; range : Eon_report.range
      }

and cblock =
  { statements : cexpression list
  ; result : cexpression option
  ; ctype : ctype
  }

val pp_cprogram : Format.formatter -> cprogram -> unit

val show_cprogram : cprogram -> string

val pp_cdefinition : Format.formatter -> cdefinition -> unit

val show_cdefinition : cdefinition -> string

val pp_ctype : Format.formatter -> ctype -> unit

val show_ctype : ctype -> string

val pp_cexpression : Format.formatter -> cexpression -> unit

val show_cexpression : cexpression -> string

val pp_cblock : Format.formatter -> cblock -> unit

val show_cblock : cblock -> string

val cexpression_type : cexpression -> ctype

val print_ctype : Format.formatter -> ctype -> unit

val cdefinition_range : cdefinition -> Eon_report.range

val cexpression_range : cexpression -> Eon_report.range
