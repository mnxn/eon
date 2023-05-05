type program = definition list [@@deriving show { with_path = false }]

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
