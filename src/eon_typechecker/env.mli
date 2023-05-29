type t

val empty : t

val add_value : string -> Typedtree.ctype -> t -> t

val add_values : (string * Typedtree.ctype) list -> t -> t

val add_type : string -> Typedtree.ctype -> t -> t

val add_types : (string * Typedtree.ctype) list -> t -> t

val lookup_value : string -> t -> Typedtree.ctype option

val lookup_type : string -> t -> Typedtree.ctype option
