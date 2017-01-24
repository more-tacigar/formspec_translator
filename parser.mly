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
  : ds = list(external_definition); EOF
    { ds }
  ;
external_definition
  : v = VARIABLE; ASSIGN; exp = expression;
    { Ast.Variable_definition (v, exp) }
  | e = IDENTIFIER; ASSIGN; LBRACE; fs = list(field); RBRACE
    { Ast.Element_definition (e, fs) }
  ;
field
  : k = IDENTIFIER; ASSIGN; v = expression
    { (k, v) }
  ;
expression
  : v = VARIABLE
    { Ast.Variable_expression v }
  | n = NUMBER
    { Ast.Number_expression n }
  | id = IDENTIFIER
    { Ast.Identifier_expression id }
  | lhs = expression; PLUS; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Plus, rhs) }
  | lhs = expression; MINUS; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Minus, rhs) }
  | lhs = expression; MULT; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Mult, rhs) }
  | lhs = expression; DIV; rhs = expression
    { Ast.Binary_operation_expression (lhs, Ast.Div, rhs) }
  | LPAREN; e = expression; RPAREN
    { Ast.Paren_expression e }
  | LBRACKET; elems = separated_list(COMMA, expression); RBRACKET
    { Ast.Array_expression elems }
  ;
