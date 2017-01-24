{
}

let variable = '@' ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let identifier = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '.']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let number = digit* frac? exp?

rule read = parse
  | [' ' '\009' '\012' '\n']+
    { read lexbuf }
  | '{'
    { Parser.LBRACE }
  | '}'
    { Parser.RBRACE }
  | '['
    { Parser.LBRACKET }
  | ']'
    { Parser.RBRACKET }
  | '('
    { Parser.LPAREN }
  | ')'
    { Parser.RPAREN }
  | ','
    { Parser.COMMA }
  | '='
    { Parser.ASSIGN }
  | '+'
    { Parser.PLUS }
  | '-'
    { Parser.MINUS }
  | '*'
    { Parser.MULT }
  | '/'
    { Parser.DIV }
  | eof
    { Parser.EOF }
  | variable
    { Parser.VARIABLE (Lexing.lexeme lexbuf) }
  | number
    { Parser.NUMBER (float_of_string (Lexing.lexeme lexbuf)) }
  | identifier
    { Parser.IDENTIFIER (Lexing.lexeme lexbuf) }
