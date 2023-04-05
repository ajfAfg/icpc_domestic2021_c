rule main = parse
  | ['0'-'9'] { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
  | "(" { Parser.LPAREN }
  | ")" { Parser.RPAREN }
  | "+" { Parser.PLUS }
  | "-" { Parser.MINUS }
  | eof { Parser.EOF }
