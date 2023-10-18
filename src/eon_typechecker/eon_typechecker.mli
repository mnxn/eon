type ('parsed, 'typed) check = 'parsed -> ('typed, Eon_report.error) result

module Primitive : sig
  val unit : Typedtree.ctype

  val bool : Typedtree.ctype

  val int : Typedtree.ctype

  val float : Typedtree.ctype

  val string : Typedtree.ctype

  val env : Env.t
end

val check_expression
  :  Env.t
  -> (Eon_parser.Parsetree.pexpression, Typedtree.cexpression) check

val check : (Eon_parser.Parsetree.pprogram, Typedtree.cprogram) check

module Typedtree = Typedtree
