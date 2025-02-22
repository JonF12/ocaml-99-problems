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

let result =
  let res = insert_at_18 8 1 [ 1; 2; 3; 4; 5; 6 ] in
  List.iter (fun x -> Printf.printf "%d" x) res
