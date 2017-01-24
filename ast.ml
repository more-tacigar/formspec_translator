type formspec = external_definition list
 and identifier = string
 and variable = string
 and element = string
 and external_definition =
  | Variable_definition of variable * expression
  | Element_definition of element * (identifier * expression) list
 and expression =
  | Variable_expression of variable
  | Number_expression of float
  | Identifier_expression of identifier
  | Binary_operation_expression of expression * binary_operator * expression
  | Paren_expression of expression
  | Array_expression of expression list
 and binary_operator =
  | Plus
  | Minus
  | Mult
  | Div
