%{
open Parsetree
%}

%start <Parsetree.pprogram> program
%start <Parsetree.pexpression> expression_eof
%%

program:
  | definitions=definition* EOF
    { definitions }

definition:
  | "func" name=IDENTIFIER "(" parameters=type_bindings ")" ":" return_type=typ "=" body=expression
    { PFunction { name; parameters; return_type; body; range = $loc } }
  | "type" name=IDENTIFIER "=" value=typ
    { PType_alias { name; value; range = $loc } }
  | "type" name=IDENTIFIER "{" fields=type_bindings "}"
    { PType_record { name; fields; range = $loc } }

type_bindings:
  | pairs=separated_list(",", separated_pair(IDENTIFIER, ":", typ))
    { pairs }

value_bindings:
  | pairs=separated_list(",", separated_pair(IDENTIFIER, "=", expression))
    { pairs }

typ:
  | name=IDENTIFIER
    { PNamed_type { name; range = $loc } }
  | "^" underlying_type=typ
    { PPointer_type { underlying_type; range = $loc } }
  | "[" element_type=typ "]"
    { PArray_type { element_type; range = $loc } }
  | "(" parameters=separated_list(",", typ) ")" "->" return_type=typ
    { PFunction_type { parameters; return_type; range = $loc } }

expression_eof:
  | e=expression EOF
    { e }

expression:
  | name=IDENTIFIER
    { PIdentifier { name; range = $loc } }
  | "(" ")"
    { PUnit { range = $loc } }
  | value=BOOLEAN
    { PBoolean { value; range = $loc } }
  | value=INTEGER
    { PInteger { value; range = $loc } }
  | value=FLOAT
    { PFloat { value; range = $loc } }
  | value=STRING
    { PString { value; range = $loc } }
  | "(" expression=expression ")"
    { PGroup { expression; range = $loc } }
  | "[" elements=separated_list(",", expression) "]"
    { PArray { elements; range = $loc } }
  | name=IDENTIFIER "{" fields=value_bindings "}"
    { PRecord { name; fields; range = $loc } }
  | expression=expression "[" index=expression "]"
    { PIndex { expression; index; range = $loc } }
  | expression=expression "." field=IDENTIFIER
    { PAccess { expression; field; range = $loc } }
  | target=expression "<-" source=expression
    { PAssign { target; source; range = $loc } }
  | func=expression "(" arguments=separated_list(",", expression) ")"
    { PApply { func; arguments; range = $loc } }
  | operator=unary_operator expression=expression
    { PUnary_operator { operator; expression; range = $loc } }
  | left=expression operator=binary_operator right=expression
    { PBinary_operator { left; operator; right; range = $loc } }
  | "{" body=block_body "}"
    { PBlock { body; range = $loc } }
  | "let" name=IDENTIFIER value_type=preceded(":", typ)? "=" value=expression
    { PLet { name; value_type; value; range = $loc } }
  | "if" condition=expression "then" true_branch=expression "else" false_branch=expression
    { PIf { condition; true_branch; false_branch; range = $loc } }
  | "func" "(" parameters=type_bindings ")" return_type=preceded(":", typ)? "->" body=expression
    { PClosure { parameters; return_type; body; range = $loc } }

block_body:
  | e=expression ";" rest=block_body
    { { rest with statements = e :: rest.statements } }
  | e=expression
    { { statements = []; result = Some e } }
  | (* empty *)
    { { statements = []; result = None } }

%inline unary_operator:
  | "!" { Not }
  | "-" { Negate }
  | "&" { Address_of }
  | "^" { Dereference }

%inline binary_operator:
  | "&&" { And }
  | "||" { Or }
  | "==" { Equal }
  | "!=" { Not_equal }
  | "<"  { Less }
  | "<=" { Less_equal }
  | ">"  { Greater }
  | ">=" { Greater_equal }
  | "+"  { Add }
  | "-"  { Subtract }
  | "*"  { Multiply }
  | "/"  { Divide }
  | "%"  { Remainder }
