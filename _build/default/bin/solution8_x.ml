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

(* 9*)
let pack_duplicates lst =
  let rec pack_duplicates_aux acc prev = function
    | [] -> List.rev acc
    | head :: rest ->
      if prev = head then
        match acc with
          | current :: others ->
            pack_duplicates_aux ((head :: current) :: others) head rest
          | [] -> pack_duplicates_aux [ [ head ] ] head rest
      else
        pack_duplicates_aux ([ head ] :: acc) head rest
  in
  match lst with
    | [] -> []
    | head :: rest -> pack_duplicates_aux [ [ head ] ] head rest
(*9 end*)

(*10*)
let encode lst =
  let rec encode_aux acc prev = function
    | [] -> List.rev acc
    | head :: rest -> (
      match acc with
        | (count, value) :: others when value = head ->
          encode_aux ((count + 1, value) :: others) head rest
        | _ -> encode_aux ((1, head) :: acc) head rest)
  in
  match lst with
    | [] -> []
    | head :: rest -> encode_aux [ (1, head) ] head rest
(*10 end*)
