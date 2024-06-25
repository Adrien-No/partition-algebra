open Lib.Labelled

let _ =

  let module D = Make(struct let k = 5 end) in
  let open D in
  print (e 4);
  factorize_right (e 2 @ e 4) 5 |> Lib.Toolbox.string_of_int_list |> Printf.printf "factorisation : %s\n"
