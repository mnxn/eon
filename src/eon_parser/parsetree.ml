type pprogram = pdefinition list [@@deriving show { with_path = false }]

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
  | PArray_type of ptype
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
