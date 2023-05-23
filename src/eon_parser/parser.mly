%{
open Ast
%}

%start <Ast.pprogram> program
%%

program:
  | definitions=definition* EOF
    { definitions }

definition:
  | "func" name=IDENTIFIER "(" parameters=type_bindings ")" ":" return_type=typ "=" body=expression
    { PFunction { name; parameters; return_type; body } }
  | "type" name=IDENTIFIER "=" value=typ
    { PType_alias { name; value } }
  | "type" name=IDENTIFIER "{" fields=type_bindings "}"
    { PType_record { name; fields } }

type_bindings:
  | pairs=separated_list(",", separated_pair(IDENTIFIER, ":", typ))
    { pairs }

value_bindings:
  | pairs=separated_list(",", separated_pair(IDENTIFIER, "=", expression))
    { pairs }

typ:
  | name=IDENTIFIER
    { PNamed_type name }
  | "^" element_type=typ
    { PPointer_type element_type }
  | "[" element_type=typ "]"
    { PSlice_type element_type }
  | "[" element_type=typ ";" length=INTEGER "]"
    { PArray_type { element_type; length } }
  | "(" parameters=separated_list(",", typ) ")" "->" return_type=typ
    { PFunction_type { parameters; return_type } }

expression:
  | id=IDENTIFIER
    { PIdentifier id }
  | "(" ")"
    { PUnit }
  | b=BOOLEAN
    { PBoolean b }
  | i=INTEGER
    { PInteger i }
  | f=FLOAT
    { PFloat f }
  | s=STRING
    { PString s }
  | "[" elements=separated_list(",", expression) "]"
    { PArray elements }
  | name=IDENTIFIER "{" fields=value_bindings "}"
    { PRecord { name; fields } }
  | expression=expression "[" index=expression "]"
    { PIndex { expression; index } }
  | expression=expression "." field=IDENTIFIER
    { PAccess { expression; field } }
  | target=expression "<-" source=expression
    { PAssign { target; source } }
  | func=expression "(" arguments=separated_list(",", expression) ")"
    { PApply { func; arguments } }
  | operator=unary_operator expression=expression
    { PUnary_operator { operator; expression } }
  | left=expression operator=binary_operator right=expression
    { PBinary_operator { left; operator; right } }
  | "{" body=block_body "}"
    { PBlock body }
  | "let" name=IDENTIFIER value_type=preceded(":", typ)? "=" value=expression
    { PLet { name; value_type; value } }
  | "if" condition=expression "then" true_branch=expression "else" false_branch=expression
    { PIf { condition; true_branch; false_branch } }
  | "func" "(" parameters=type_bindings ")" return_type=preceded(":", typ)? "->" body=expression
    { PClosure { parameters; return_type; body } }

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
