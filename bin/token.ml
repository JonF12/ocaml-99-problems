type string_opt = Open | Close

type token =
  | OpenArray
  | CloseArray
  | OpenNode
  | CloseNode
  | Comma
  | Colon
  | StringQuote of string_opt
  | StringProperty of string
  | StringValue of string
