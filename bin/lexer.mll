{
  open Token
}
rule token = parse
  | [' ' '\t']
    { token lexbuf }
  | '\n'
    { Lexing.new_line lexbuf; token lexbuf }
  | '\r' '\n'
    { Lexing.new_line lexbuf; token lexbuf }
  | '['
    { OpenArray (Lexing.lexeme_start_p lexbuf) }
  | ']'
    { CloseArray (Lexing.lexeme_start_p lexbuf) }
  | '{'
    { OpenNode (Lexing.lexeme_start_p lexbuf) }
  | '}'
    { CloseNode (Lexing.lexeme_start_p lexbuf) }
  | ','
    { Comma (Lexing.lexeme_start_p lexbuf) }
  | ':'
    { Colon (Lexing.lexeme_start_p lexbuf) }
  | ['0'-'9']+ as lxm
    { IntValue(int_of_string lxm, Lexing.lexeme_start_p lexbuf) }
  | "null"
    { NullValue (Lexing.lexeme_start_p lexbuf) }
  | "true"
    { BoolValue(true, Lexing.lexeme_start_p lexbuf) }
  | "false"
    { BoolValue(false, Lexing.lexeme_start_p lexbuf) }
  | '"'
    { read_string (Buffer.create 16) (Lexing.lexeme_start_p lexbuf) lexbuf }
  | eof
    { EOF }
  | _ as c
    { let pos = Lexing.lexeme_start_p lexbuf in
      Printf.printf "Unhandled character at line %d, column %d: %c\n" 
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) c;
      raise (Failure (Printf.sprintf "Unexpected character at line %d, column %d: %c"
        pos.pos_lnum (pos.pos_cnum - pos.pos_bol) c)) }

and read_string buffer start_pos = parse
  | '"'
    { StringValue(Buffer.contents buffer, start_pos) }
  | '\\' '"'
    { Buffer.add_char buffer '"'; read_string buffer start_pos lexbuf }
  | '\\' '\\'
    { Buffer.add_char buffer '\\'; read_string buffer start_pos lexbuf }
  | '\\' 'n'
    { Buffer.add_char buffer '\n'; read_string buffer start_pos lexbuf }
  | '\\' 'r'
    { Buffer.add_char buffer '\r'; read_string buffer start_pos lexbuf }
  | '\\' 't'
    { Buffer.add_char buffer '\t'; read_string buffer start_pos lexbuf }
  | [^ '"' '\\']+ as str
    { Buffer.add_string buffer str; read_string buffer start_pos lexbuf }
  | eof
    { let pos = Lexing.lexeme_start_p lexbuf in
      raise (Failure (Printf.sprintf "Unterminated string starting at line %d, column %d"
        start_pos.pos_lnum (start_pos.pos_cnum - start_pos.pos_bol))) }
