type cprogram = cdefinition list [@@deriving show { with_path = false }]

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
      ; record : crecord
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
  | CBlock { ctype; _ }
  | CLet { ctype; _ }
  | CIf { ctype; _ }
  | CClosure { ctype; _ } -> ctype

let rec print_ctype ppf = function
  | CPrimitive_type name -> Format.fprintf ppf "%s" name
  | CPointer_type underlying -> Format.fprintf ppf "^%a" print_ctype underlying
  | CArray_type element_type -> Format.fprintf ppf "[%a]" print_ctype element_type
  | CFunction_type { parameters = [ param ]; return_type } ->
    Format.fprintf ppf "%a -> %a" print_ctype param print_ctype return_type
  | CFunction_type { parameters; return_type } ->
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
