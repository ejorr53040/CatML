{
open Parser
}

let white = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | digit | '_')*

rule read =
  parse
  | white { read lexbuf }
  | "True" { TRUE }
  | "False" { FALSE }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "," { COMMA }
  | "->" { ARROW }
  | "." { DOT }
  | "Let" { LET }
  | "=" { EQUALS }
  | "In" { IN }
  | "If" { IF }
  | "Then" { THEN }
  | "Else" { ELSE }
  | "And" { AND }
  | "Or" { OR }
  | "Not" { NOT }
  | "First" { FIRST }
  | "Second" { SECOND }
  | "Fun" { FUN }
  | "Fix" { FIX }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
