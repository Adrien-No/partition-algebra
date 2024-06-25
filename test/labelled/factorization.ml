open Lib.Labelled

let _ =

  let module D = Make(struct let k = 5 end) in
  let open D in
  factorize_right (e 1 @ e 1 @ e 2 @ e 3 @ e 1) 5 |> Lib.Toolbox.string_of_int_list |> Printf.printf "factorisation : %s\n"
