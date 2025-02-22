let rec product lst = List.fold_left ( * ) 1 lst

 let rec str_concat = function
  | [] -> ""
  | h :: t -> h ^ str_concat t

let has_bigred = function
  | "bigred" :: _ -> true
  | _ -> false

let two_or_four = function
  | [ _; _; _; _ ] -> true
  | [ _; _ ] -> true
  | _ -> false

let first_two_equal = function
  | x :: y :: _ -> x = y
  | _ -> false

let rec sum_lines accum =
  match int_of_string_opt (read_line ()) with
    | Some value -> sum_lines (value + accum)
    | None -> accum

let print_square () =
  let rec print_square_aux x y =
    match (x, y) with
      | 10, 10 -> print_string "*\n"
      | 10, y ->
        print_string "*\n";
        print_square_aux 1 (y + 1)
      | x, 1 ->
        print_string "*";
        print_square_aux (x + 1) y
      | 1, y ->
        print_string "*";
        print_square_aux (x + 1) y
      | x, 10 ->
        print_string "*";
        print_square_aux (x + 1) y
      | _ ->
        print_string " ";
        print_square_aux (x + 1) y
  in
  print_square_aux 1 1

let rec last_element lst =
  let result = List.rev lst in
  List.nth result 0

let sort_descending lst = List.sort (fun x y -> y - x) lst

let take_list lst n =
  let rec take_list_aux acc count = function
    | [] -> acc
    | h :: t when count < n -> h :: take_list_aux acc (count + 1) t
    | _ -> acc
  in
  take_list_aux [] 0 lst

let drop_list lst n =
  let rec drop_list_aux count = function
    | [] -> []
    | t when count >= n -> t
    | h :: t -> drop_list_aux (count + 1) t
  in
  drop_list_aux 0 lst
