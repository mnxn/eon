%{
open Ast
%}

%start <Ast.program> program
%%

program:
  | definitions=definition* EOF
    { definitions }

definition:
  | "func" name=IDENTIFIER "(" parameters=type_bindings ")" ":" return_type=typ "=" body=expression
    { Function { name; parameters; return_type; body } }
  | "type" name=IDENTIFIER "=" typ=typ
    { Type_alias { name; typ } }
  | "type" name=IDENTIFIER "{" fields=type_bindings "}"
    { Type_record { name; fields } }

type_bindings:
  | pairs=separated_list(",", separated_pair(IDENTIFIER, ":", typ))
    { pairs }

value_bindings:
  | pairs=separated_list(",", separated_pair(IDENTIFIER, "=", expression))
    { pairs }

typ:
  | name=IDENTIFIER
    { Named_type name }
  | "^" element_type=typ
    { Pointer_type element_type }
  | "[" element_type=typ "]"
    { Slice_type element_type }
  | "[" element_type=typ ";" length=INTEGER "]"
    { Array_type { element_type; length } }
  | "(" parameters=separated_list(",", typ) ")" "->" return_type=typ
    { Function_type { parameters; return_type } }

expression:
  | id=IDENTIFIER
    { Identifier id }
  | "(" ")"
    { Unit }
  | b=BOOLEAN
    { Boolean b }
  | i=INTEGER
    { Integer i }
  | f=FLOAT
    { Float f }
  | s=STRING
    { String s }
  | "[" elements=separated_list(",", expression) "]"
    { Array elements }
  | name=IDENTIFIER "{" fields=value_bindings "}"
    { Record { name; fields } }
  | expression=expression "[" index=expression "]"
    { Index { expression; index } }
  | expression=expression "." field=IDENTIFIER
    { Access { expression; field } }
  | target=expression "<-" source=expression
    { Assign { target; source } }
  | func=expression "(" arguments=separated_list(",", expression) ")"
    { Apply { func; arguments } }
  | operator=unary_operator expression=expression
    { Unary_operator { operator; expression } }
  | left=expression operator=binary_operator right=expression
    { Binary_operator { left; operator; right } }
  | "{" body=block_body "}"
    { Block body }
  | "let" name=IDENTIFIER typ=preceded(":", typ)? "=" value=expression
    { Let { name; typ; value } }
  | "if" condition=expression "then" true_branch=expression "else" false_branch=expression
    { If { condition; true_branch; false_branch } }
  | "func" "(" parameters=type_bindings ")" return_type=preceded(":", typ)? "->" body=expression
    { Closure { parameters; return_type; body } }

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
