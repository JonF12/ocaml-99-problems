(* 1 *)
let tail_of_list lst =
  let rec tail_of_list_aux = function
    | [] -> None
    | h :: [] -> Some h
    | h :: t -> tail_of_list_aux t
  in
    tail_of_list_aux lst

(* 2 *)
let last_two_of_list lst =
  let rec last_two_of_list_aux = function
    | [] -> None
    | [ h1 ] -> None
    | [ h1; h2 ] -> Some [ h1; h2 ]
    | h :: t -> last_two_of_list_aux t
  in
    last_two_of_list_aux lst

(* 3 *)
let nth_of_list n lst =
  let rec nth_of_list_aux lst count =
    match lst with
      | [] -> None
      | h :: t ->
        if count = n then
          Some h
        else
          nth_of_list_aux t (count + 1)
  in
    nth_of_list_aux lst 0

(* 4 *)
let length_of_list lst =
  let rec length_of_list_aux acc = function
    | [] -> acc
    | h :: t -> length_of_list_aux (acc + 1) t
  in
    length_of_list_aux 0 lst

(* 5 *)
let reverse_list lst =
  let rec reverse_list_aux acc = function
    | [] -> acc
    | h :: t -> reverse_list_aux (h :: acc) t
  in
    reverse_list_aux [] lst

(* 6 *)
let is_palindrome lst = lst = reverse_list lst

let () =
  let result = is_palindrome [ 1; 1; 2; 1 ] in
    Printf.printf "\n RESULT:%b\n\n" result
