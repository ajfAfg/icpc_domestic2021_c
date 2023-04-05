%{
open Syntax
%}

%token LPAREN RPAREN
%token PLUS MINUS
%token EOF

%token <int> INTV

%start root
%type <Syntax.exp> root
%%

root :
  | e1=non_root op1=op e2=non_root op2=op e3=non_root
    { if op1 = op2 then Op (op1, [e1; e2; e3]) else failwith "op1 and op2 should be equal" }
  | EOF
    { exit 0 }
  | { failwith "Syntax error" }

non_root :
  | i=INTV
    { ILit i }
  | LPAREN e1=non_root op=op e2=non_root RPAREN
    { Op (op, [e1; e2]) }

%inline op :
  | PLUS
    { Plus }
  | MINUS
    { Minus }
