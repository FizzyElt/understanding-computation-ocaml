open Lib.Algebra

let () =
  let expression = Add (Num 1, Add (Num 2, Num 3)) in
  run expression print_endline
