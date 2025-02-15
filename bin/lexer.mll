{
  open Token
}

rule token = parse
  [' ' '\t' '\n' '\r']
  | ['"']{ StringQuote }
  | [':'] { Colon }
  | [','] { Comma }
  | ['{'] { Comma }

