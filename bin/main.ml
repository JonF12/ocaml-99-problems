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
    | [ h1; h2 ] -> Some [ h1; h2 ]
    | h :: t -> last_two_of_list_aux t
  in
  last_two_of_list_aux lst

let () =
  let result = last_two_of_list [ 2; 2; 2; 2; 3 ] in
  match result with
  | None -> Printf.printf "No value found\n"
  | Some [ x; y ] -> Printf.printf "val: %d %d\n" x y
  | Some _ -> Printf.printf "Unexpected length"
