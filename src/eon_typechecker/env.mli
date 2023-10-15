type t

type 'a binding_type =
  | Type : Typedtree.ctype binding_type
  | Value : Typedtree.ctype binding_type
  | Record : Typedtree.crecord binding_type

val empty : t

val add : 'a binding_type -> string -> 'a -> t -> t

val add_all : 'a binding_type -> (string * 'a) list -> t -> t

val lookup : 'a binding_type -> string -> t -> 'a option
