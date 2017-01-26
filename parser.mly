%token<float>  NUMBER
%token<string> VARIABLE
%token<string> IDENTIFIER
%token PLUS MINUS MULT DIV ASSIGN
%token LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE COMMA
%token EOF

%left PLUS MINUS
%left MULT DIV

%start formspec
%type<Ast.formspec> formspec

%%

formspec
  : external_definitions EOF
    { List.rev $1 }
  ;
external_definitions
  : external_definitions external_definition
    { $2 :: $1 }
  | external_definition
    { [$1] }
  ;
external_definition
  : VARIABLE ASSIGN expression;
    { Ast.Variable_definition ($1, $3) }
  | IDENTIFIER ASSIGN LBRACE fields RBRACE
    { Ast.Element_definition ($1, $4) }
  ;
fields
  : fields field
    { $2 :: $1 }
  | field
    { [$1] }
  ;
field
  : IDENTIFIER ASSIGN expression
    { ($1, $3) }
  ;
expression
  : VARIABLE
    { Ast.Variable_expression $1 }
  | NUMBER
    { Ast.Number_expression $1 }
  | IDENTIFIER
    { Ast.Identifier_expression $1 }
  | expression PLUS expression
    { Ast.Binary_operation_expression ($1, Ast.Plus, $3) }
  | expression MINUS expression
    { Ast.Binary_operation_expression ($1, Ast.Minus, $3) }
  | expression MULT expression
    { Ast.Binary_operation_expression ($1, Ast.Mult, $3) }
  | expression DIV expression
    { Ast.Binary_operation_expression ($1, Ast.Div, $3) }
  | LPAREN expression RPAREN
    { Ast.Paren_expression $2 }
  | LBRACKET expressions RBRACKET
    { Ast.Array_expression (List.rev $2) }
  ;
expressions
  : expressions COMMA expression
    { $3 :: $1 }
  | expression
    { [$1] }
  ;
