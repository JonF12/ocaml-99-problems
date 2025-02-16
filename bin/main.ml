open Token

let get_token_list lexbuf =
  let rec get_token_list_aux acc =
    match Lexer.token lexbuf with
      | EOF -> acc
      | t -> get_token_list_aux (t :: acc)
  in
  List.rev (get_token_list_aux [])

let () =
  let lexbuf =
    Lexing.from_string
      "{\"name\": \"John\", \"age\": 30, \"array\": [1, 2, null]}"
  in
  let token_list = get_token_list lexbuf in
  List.map string_of_token token_list |> List.iter (Printf.printf "%s\n")
