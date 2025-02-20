type token =
  | OpenArray of Lexing.position
  | CloseArray of Lexing.position
  | OpenNode of Lexing.position
  | CloseNode of Lexing.position
  | Comma of Lexing.position
  | Colon of Lexing.position
  | StringValue of string * Lexing.position
  | IntValue of int * Lexing.position
  | BoolValue of bool * Lexing.position
  | NullValue of Lexing.position
  | EOF

let string_of_token = function
  | OpenArray x -> "OpenArray"
  | CloseArray _ -> "CloseArray"
  | OpenNode _ -> "OpenNode"
  | CloseNode _ -> "CloseNode"
  | Comma _ -> "Comma"
  | Colon _ -> "Colon"
  | StringValue (str, _) -> "StringValue(\"" ^ str ^ "\")"
  | IntValue (n, _) -> "IntValue(" ^ string_of_int n ^ ")"
  | BoolValue (b, _) -> "BoolValue(" ^ string_of_bool b ^ ")"
  | NullValue _ -> "NullValue"
  | EOF -> "EOF"
