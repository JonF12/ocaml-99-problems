type string_option = Start | End

type token =
  | NodeStart
  | NodeEnd
  | ArrayStart
  | ArrayEnd
  | Colon
  | String
  | Comma

let token_to_string = function
  | NodeStart -> "NodeStart"
  | NodeEnd -> "NodeEnd"
  | ArrayStart -> "ArrayStart"
  | ArrayEnd -> "ArrayEnd"
  | Colon -> "Colon"
  | String -> "String"
  | Comma -> "Comma"

let token_map = function
  | '{' -> Some NodeStart
  | '}' -> Some NodeEnd
  | '[' -> Some ArrayStart
  | ']' -> Some ArrayEnd
  | ':' -> Some Colon
  | ',' -> Some Comma
  | '"' -> Some String
  | _ -> None

let lex_json str =
  let len = String.length str in
  let pos = ref 0 in

  let peek () =
    if !pos < len then
      Some (String.get str !pos)
    else
      None
  in

  let advance () = pos := !pos + 1 in

  let tokens = ref [] in
  while !pos < len do
    match peek () with
      | Some char -> (
        let token = token_map char in
        match token with
          | Some token ->
            tokens := token :: !tokens;
            advance ()
          | None ->
            ();
            advance ())
      | None ->
        ();
        advance ()
  done;
  List.rev !tokens
