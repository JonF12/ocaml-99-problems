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
(*11 RUN LENGTH ENCODING (good version) *)
type 'a rle = One of 'a | Many of int * 'a

let to_rle group =
  match Seq.uncons group with
    | Some (first, _) ->
      if Seq.length group > 1 then
        Many (Seq.length group, first)
      else
        One first
    | None -> failwith "Empty group encountered"

let encode lst =
  lst |> List.to_seq |> Seq.group ( = ) |> Seq.map to_rle |> List.of_seq
(*11 end*)

(*12 decode rle*)
let from_rle = function
  | Many (count, value) -> List.init count (fun _ -> value)
  | One value -> [ value ]

let decode lst = List.map from_rle lst |> List.concat
let duplicate_11 lst = List.map (fun x -> [ x; x ]) lst |> List.concat

let duplicate_12 lst ct =
  List.map (fun x -> List.init ct (fun y -> x)) lst |> List.concat

let drop_nth_13 lst n = List.filteri (fun x i -> i mod n <> 0) lst
let split_length_14 lst n = (List.take n lst, List.drop n lst)

let slice_15 lst i j =
  if i > j then raise (Invalid_argument "invalid arg");
  List.drop i lst |> List.take (j - i)

let rotate_16 lst n =
  if List.length lst = 0 || n = 0 then
    lst
  else
    let rot = n mod (List.length lst - 1) in
    if rot = 0 then
      lst
    else
      List.drop rot lst @ List.take rot lst

let remove_at_17 lst n = List.filteri (fun x i -> i - 1 <> n) lst

let insert_at_18 item i lst =
  if List.length lst - 1 < i || i < 0 then
    raise (Invalid_argument "invalid arg");
  List.take i lst @ [ item ] @ List.drop i lst

(** bunch of solutions ez im not writing tail recursive for all of these LOL **)
