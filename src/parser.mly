%{
open Lang
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF

%nonassoc IN
%nonassoc ELSE
%left EQUALS
%left PLUS
%left TIMES

%start <Lang.expr> prog

%%

prog:
  | e = expr; EOF { e }
  ;

expr:
  | i    = INT { Int i }
  | b    = BOOL { Bool b }
  | x    = ID  { Var x }
  | TRUE       { Bool true }
  | FALSE      { Bool false }
  | e1 = expr; EQUALS; e2 = expr { Equal (e1, e2) }
  | e1 = expr; TIMES;  e2 = expr { Mult(e1, e2) }
  | e1 = expr; PLUS;   e2 = expr { Plus(e1, e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr   { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | LPAREN; e=expr; RPAREN {e}
  | And; e1 = expr; e2 = expr { And (e1, e2) }
  | Or; e1 = expr; e2 = expr { Or (e1, e2) }
  | Not; e = expr { Not e }
  | Pair; e1 = expr; e2 = expr { Pair (e1, e2) }
  | First; e = expr { First e }
  | Second; e = expr { Second e }
  | Fun; x = ID; e = expr { Fun (x, e) }
  | Fix; x = ID; e = expr { Fix (x, e) }
  | App; e1 = expr; e2 = expr { App (e1, e2) }
  ;