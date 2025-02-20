open Printf

let lst_and_lib = List.for_all (fun x -> x)

let rec create_list (i : int) (j : int) =
  if i > j then
    []
  else
    i :: create_list (i + 1) j

let () =
  let result = create_list 0 10 |> List.map (fun x -> x * x) in
  List.iter (fun x -> printf "%d," x) result

(*let has_tc = has_trailing_comma token_list in*)
(*List.iter*)
(*  (fun (Some token, pos) ->*)
(*    Printf.printf "Trailing comma found at %s: %s\n" (string_of_position pos)*)
(*      (string_of_token token))*)
(*  has_tc*)
