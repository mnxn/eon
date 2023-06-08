type cprogram = cdefinition list

and cdefinition =
  | CFunction of
      { name : string
      ; parameters : (string * ctype) list
      ; return_type : ctype
      ; body : cexpression
      }
  | CType_alias of
      { name : string
      ; value : ctype
      }
  | CType_record of
      { name : string
      ; fields : (string * ctype) list
      }

and ctype =
  | CPrimitive_type of string
  | CPointer_type of ctype
  | CArray_type of ctype
  | CFunction_type of
      { parameters : ctype list
      ; return_type : ctype
      }
  | CRecord_type of
      { name : string
      ; fields : (string * ctype) list
      }

and cexpression =
  | CIdentifier of
      { name : string
      ; ctype : ctype
      }
  | CUnit of { ctype : ctype }
  | CBoolean of
      { value : bool
      ; ctype : ctype
      }
  | CInteger of
      { value : int64
      ; ctype : ctype
      }
  | CFloat of
      { value : float
      ; ctype : ctype
      }
  | CString of
      { value : string
      ; ctype : ctype
      }
  | CArray of
      { elements : cexpression list
      ; ctype : ctype
      }
  | CRecord of
      { name : string
      ; fields : (string * cexpression) list
      ; ctype : ctype
      }
  | CIndex of
      { expression : cexpression
      ; index : cexpression
      ; ctype : ctype
      }
  | CAccess of
      { expression : cexpression
      ; field : string
      ; ctype : ctype
      }
  | CAssign of
      { target : cexpression
      ; source : cexpression
      ; ctype : ctype
      }
  | CApply of
      { func : cexpression
      ; arguments : cexpression list
      ; ctype : ctype
      }
  | CUnary_operator of
      { operator : Eon_parser.Parsetree.unary_operator
      ; expression : cexpression
      ; ctype : ctype
      }
  | CBinary_operator of
      { left : cexpression
      ; operator : Eon_parser.Parsetree.binary_operator
      ; right : cexpression
      ; ctype : ctype
      }
  | CBlock of cblock
  | CLet of
      { name : string
      ; value_type : ctype
      ; value : cexpression
      ; ctype : ctype
      }
  | CIf of
      { condition : cexpression
      ; true_branch : cexpression
      ; false_branch : cexpression
      ; ctype : ctype
      }
  | CClosure of
      { parameters : (string * ctype) list
      ; return_type : ctype
      ; body : cexpression
      ; ctype : ctype
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
