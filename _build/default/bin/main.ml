let multiply_var pred f x =
  if pred > 5 then
    f x pred
  else
    f x 2

let multiply x y = x * y

let () =
  let result = multiply_var 6 multiply 3 in
  Printf.printf "%d" result
