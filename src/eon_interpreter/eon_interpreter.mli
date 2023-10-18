open Eon_typechecker

module Env : Map.S with type key = string

type value =
  | Unit
  | Boolean of bool
  | Integer of int64
  | Float of float
  | String of string
  | Array of value array
  | Record of
      { name : string
      ; fields : (string, value) Hashtbl.t
      }
  | Box of { mutable value : value }
  | Closure of
      { parameters : string array
      ; body : Typedtree.cexpression
      ; env : value Env.t Lazy.t
      }

val print_value : Format.formatter -> value -> unit

val equal_value : value -> value -> bool

exception
  Runtime_exception of
    { runtime_error : Eon_report.runtime_error
    ; range : Eon_report.range option
    }

val lookup_value : value Env.t -> ?range:Eon_report.range -> Env.key -> value

val eval_expression : value Env.t -> Typedtree.cexpression -> value

val run : value Env.t -> Typedtree.cdefinition list -> value

val try_eval
  :  (value Env.t -> 'a -> 'b)
  -> value Env.t
  -> 'a
  -> ('b, Eon_report.error) result
