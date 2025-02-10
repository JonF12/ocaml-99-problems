(** requires: [n >= 0].*)
let rec factorial n =
  match n with
  | 0 | 1 -> 1 
  | n -> n * factorial (n - 1)

let () =
  let result = factorial 2 in
  Printf.printf "val: %d\n" result
