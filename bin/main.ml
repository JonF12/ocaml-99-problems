open Printf

let print_matrix m =
  Array.iter
    (fun row ->
       Array.iter (fun element -> Printf.printf "%d " element) row;
       Printf.printf "\n")
    m
;;

let init_matrix rows cols f =
  Array.init rows (fun i -> Array.init cols (fun j -> f i j))
;;

let result = Array.init_matrix 3 4 (fun _ _ -> 9) |> print_matrix
