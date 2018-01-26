{
  open FromStringParser
}

rule tokens = parse
  | ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9']* { SYMSTRING(Lexing.lexeme lexbuf) }
  | ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* { SCHSTRING(Lexing.lexeme lexbuf) }
  | ['0'-'9']+ { INT(int_of_string (Lexing.lexeme lexbuf)) }
  | '@' { AT }
  | '(' { LPAR }
  | ')' { RPAR }
  | '|' { PIPE }
  | "->" { ARROW }
  | eof { EOF }
  | _ { tokens lexbuf }
