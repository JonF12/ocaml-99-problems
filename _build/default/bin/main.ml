let to_digit n =
  let rec to_digit_aux acc num =
    match num with
      | 0 when acc = [] -> [ "0" ]
      | 0 -> acc
      | n ->
        let digit = string_of_int (n mod 10) in
          to_digit_aux (digit :: acc) (n / 10)
  in
    String.concat "-" (to_digit_aux [] n)

let () =
  Printf.printf "Running: \n\n";
  let result = to_digit 90994123909 in
    Printf.printf "%s" result
(*Printf.printf "\n";*)
(*List.iter (fun x -> Printf.printf "%d" x) result;*)
(*Printf.printf "\n"*)
