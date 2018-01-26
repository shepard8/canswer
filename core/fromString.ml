let symbol s =
  FromStringParser.symbol FromStringLexer.tokens (Lexing.from_string s)

let relation_name s =
  FromStringParser.relation_name FromStringLexer.tokens (Lexing.from_string s)

let substitution s =
  FromStringParser.substitution FromStringLexer.tokens (Lexing.from_string s)

let atom s =
  FromStringParser.atom FromStringLexer.tokens (Lexing.from_string s)

let conjunctive s =
  FromStringParser.conjunctive FromStringLexer.tokens (Lexing.from_string s)

