type t

type binding_type =
  | Type
  | Value

val empty : t

val add : binding_type -> string -> Typedtree.ctype -> t -> t

val add_all : binding_type -> (string * Typedtree.ctype) list -> t -> t

val lookup : binding_type -> string -> t -> Typedtree.ctype option
