(* 8 START*)
let has_item lst n =
  let rec has_item_aux n = function
    | [] -> false
    | head :: rest -> head = n || has_item_aux n rest
  in
    has_item_aux n lst

let compress lst =
  let rec compress_aux new_lst = function
    | [] -> []
    | head :: rest ->
      if has_item new_lst head then
        compress_aux new_lst rest
      else
        head :: compress_aux (head :: new_lst) rest
  in
    compress_aux [] lst
(* 8 END *)
