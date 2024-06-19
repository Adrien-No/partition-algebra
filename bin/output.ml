let draw_diagram () =
  (* draw diagrams *)
  let c = ref 0 in
  let continue = ref true in
  while !continue do
      try
        (* let justify = 10 + ((log (float_of_int !c) /. (log 2.) |> int_of_float) + 1) in *)
        Lib.Draw.pin_dot_and_typo (Sys.getcwd() ^ Printf.sprintf "/img/diagram%i.dot" !c);
        incr c
      with Sys_error _s -> (* Printf.printf "dernier fichier+1: %s\n" _s; *)
        continue := false
  done
