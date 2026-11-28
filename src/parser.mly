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
%token COMMA
%token ARROW
%token DOT
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token AND
%token OR
%token NOT
%token FIRST
%token SECOND
%token FUN
%token FIX
%token EOF

%start <Lang.expr> prog

%%

(* One nonterminal per precedence level, loosest first. Let, If, Fun and Fix
   extend as far right as possible, so they only appear at the top level or
   inside parentheses. Binary operators are left-associative, and
   application (e e) binds tighter than every operator. *)

prog:
  | e = expr; EOF { e }
  ;

expr:
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr   { Let (x, e1, e2) }
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
  | FUN; x = ID; ARROW; e = expr                    { Fun (x, e) }
  | FIX; f = ID; DOT; x = ID; ARROW; e = expr       { Fix (f, x, e) }
  | e = or_expr                                     { e }
  ;

or_expr:
  | e1 = or_expr; OR; e2 = and_expr { Or (e1, e2) }
  | e = and_expr                    { e }
  ;

and_expr:
  | e1 = and_expr; AND; e2 = eq_expr { And (e1, e2) }
  | e = eq_expr                      { e }
  ;

eq_expr:
  | e1 = eq_expr; EQUALS; e2 = plus_expr { Equal (e1, e2) }
  | e = plus_expr                        { e }
  ;

plus_expr:
  | e1 = plus_expr; PLUS; e2 = times_expr { Plus (e1, e2) }
  | e = times_expr                        { e }
  ;

times_expr:
  | e1 = times_expr; TIMES; e2 = unary_expr { Mult (e1, e2) }
  | e = unary_expr                          { e }
  ;

unary_expr:
  | NOT; e = unary_expr    { Not e }
  | FIRST; e = unary_expr  { First e }
  | SECOND; e = unary_expr { Second e }
  | e = app_expr           { e }
  ;

app_expr:
  | e1 = app_expr; e2 = atom { App (e1, e2) }
  | e = atom                 { e }
  ;

atom:
  | i = INT                                     { Int i }
  | x = ID                                      { Var x }
  | TRUE                                        { Bool true }
  | FALSE                                       { Bool false }
  | LPAREN; e = expr; RPAREN                    { e }
  | LPAREN; e1 = expr; COMMA; e2 = expr; RPAREN { Pair (e1, e2) }
  ;
