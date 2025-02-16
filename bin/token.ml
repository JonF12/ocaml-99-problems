type string_opt = Open | Close

type token =
  | OpenArray
  | CloseArray
  | OpenNode
  | CloseNode
  | Comma
  | Colon
  | StringValue of string
  | IntValue of int
  | BoolValue of bool
  | NullValue
  | EOF

let string_of_token = function
  | OpenArray -> "OpenArray"
  | CloseArray -> "CloseArray"
  | OpenNode -> "OpenNode"
  | CloseNode -> "CloseNode"
  | Comma -> "Comma"
  | Colon -> "Colon"
  | StringValue str -> "StringValue(\"" ^ str ^ "\")"
  | IntValue n -> "IntValue(" ^ string_of_int n ^ ")"
  | BoolValue b -> "BoolValue(" ^ string_of_bool b ^ ")"
  | NullValue -> "NullValue"
  | EOF -> "EOF"
