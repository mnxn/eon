%token EOF

%token <string> IDENTIFIER
%token <bool>   BOOLEAN
%token <int64>  INTEGER
%token <float>  FLOAT
%token <string> STRING

%token LET "let" FUNC "func" TYPE "type"
%token IF "if"  THEN "then" ELSE "else"

%token SEMICOLON ";" COLON ":" DOT "." COMMA ","
%token AMPERSAND "&" HAT "^"
%token NOT "!" AND "&&" OR "||"
%token EQUAL "=" EQUAL_EQUAL "==" NOT_EQUAL "!="
%token LESS "<" LESS_EQUAL "<="
%token GREATER ">" GREATER_EQUAL ">="
%token PLUS "+" DASH "-" ASTERISK "*" SLASH "/" PERCENT "%"
%token LARROW "<-" RARROW "->"

%token LPAREN "(" RPAREN ")"
%token LBRACKET "[" RBRACKET "]"
%token LCURLY "{" RCURLY "}"

%left EQUAL
%left RARROW

%left ELSE

%left LARROW
%left OR
%left AND
%left EQUAL_EQUAL NOT_EQUAL
      LESS LESS_EQUAL
      GREATER GREATER_EQUAL
%left PLUS DASH
%left ASTERISK SLASH PERCENT

%left NOT
%left AMPERSAND
%left HAT

%left LPAREN
%left LBRACKET
%left DOT
%%
