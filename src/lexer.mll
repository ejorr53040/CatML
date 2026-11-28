{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read =
  parse
  | white { read lexbuf }
  | "True" { TRUE }
  | "False" { FALSE }
  | "*" { TIMES }
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "Let" { LET }
  | "=" { EQUALS }
  | "In" { IN }
  | "If" { IF }
  | "Then" { THEN }
  | "Else" { ELSE }
  | "And" { And }
  | "Or" { Or }
  | "Not" { Not }
  | "Pair" { Pair }
  | "First" { First }
  | "Second" { Second }
  | "Fun" { Fun }
  | "Fix" { Fix }
  | "App" { App }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
