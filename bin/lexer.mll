{
  open Token
}
rule token = parse
  [' ' '\t' '\n' '\r']
    { token lexbuf }
  | ['[']
    { OpenArray }
  | [']']
    { CloseArray }
  | ['{']
    { OpenNode }
  | ['}']
    { CloseNode }
  | [',']
    { Comma }
  | [':']
    { Colon }
  | ['0'-'9']+ as lxm
    { IntValue(int_of_string lxm) }
  | "null"
    { NullValue }
  | "true"
    { BoolValue(true) }
  | "false"
    { BoolValue(false) }
  | '"'
    { read_string (Buffer.create 16) lexbuf }
  | eof
    { EOF }
  | _ as c
    { Printf.printf "Unhandled character: %c\n" c;
      raise (Failure ("Unexpected character: " ^ String.make 1 c)) }

and read_string buffer = parse
  | '"'
    { StringValue(Buffer.contents buffer) }
  | '\\' '"'
    { Buffer.add_char buffer '"'; read_string buffer lexbuf }
  | '\\' '\\'
    { Buffer.add_char buffer '\\'; read_string buffer lexbuf }
  | '\\' 'n'
    { Buffer.add_char buffer '\n'; read_string buffer lexbuf }
  | '\\' 'r'
    { Buffer.add_char buffer '\r'; read_string buffer lexbuf }
  | '\\' 't'
    { Buffer.add_char buffer '\t'; read_string buffer lexbuf }
  | [^ '"' '\\']+ as str
    { Buffer.add_string buffer str; read_string buffer lexbuf }
  | eof
    { raise (Failure "Unterminated string") }
