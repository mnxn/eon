type cprogram = cdefinition list [@@deriving show { with_path = false }]

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

let cexpression_type (cexpr : cexpression) =
  match cexpr with
  | CIdentifier { ctype; _ }
  | CUnit { ctype; _ }
  | CBoolean { ctype; _ }
  | CInteger { ctype; _ }
  | CFloat { ctype; _ }
  | CString { ctype; _ }
  | CArray { ctype; _ }
  | CRecord { ctype; _ }
  | CIndex { ctype; _ }
  | CAccess { ctype; _ }
  | CAssign { ctype; _ }
  | CApply { ctype; _ }
  | CUnary_operator { ctype; _ }
  | CBinary_operator { ctype; _ }
  | CBlock { block = { ctype; _ }; _ }
  | CLet { ctype; _ }
  | CIf { ctype; _ }
  | CClosure { ctype; _ } -> ctype

let rec print_ctype ppf = function
  | CPrimitive_type name -> Format.fprintf ppf "%s" name
  | CPointer_type underlying -> Format.fprintf ppf "^%a" print_ctype underlying
  | CArray_type element_type -> Format.fprintf ppf "[%a]" print_ctype element_type
  | CFunction_type { parameters = [ param ]; return_type; _ } ->
    Format.fprintf ppf "%a -> %a" print_ctype param print_ctype return_type
  | CFunction_type { parameters; return_type; _ } ->
    Format.fprintf ppf "(";
    print_function_parameters ppf parameters;
    Format.fprintf ppf ") -> %a" print_ctype return_type
  | CRecord_type { name; _ } -> Format.fprintf ppf "%s" name

and print_function_parameters ppf = function
  | [] -> ()
  | [ param ] -> print_ctype ppf param
  | param :: params ->
    Format.fprintf ppf "%a, " print_ctype param;
    print_function_parameters ppf params

let cdefinition_range : cdefinition -> Eon_report.range = function
  | CFunction { range; _ } | CType_alias { range; _ } | CType_record { range; _ } -> range

let cexpression_range : cexpression -> Eon_report.range = function
  | CIdentifier { range; _ }
  | CUnit { range; _ }
  | CBoolean { range; _ }
  | CInteger { range; _ }
  | CFloat { range; _ }
  | CString { range; _ }
  | CArray { range; _ }
  | CRecord { range; _ }
  | CIndex { range; _ }
  | CAccess { range; _ }
  | CAssign { range; _ }
  | CApply { range; _ }
  | CUnary_operator { range; _ }
  | CBinary_operator { range; _ }
  | CBlock { range; _ }
  | CLet { range; _ }
  | CIf { range; _ }
  | CClosure { range; _ } -> range
