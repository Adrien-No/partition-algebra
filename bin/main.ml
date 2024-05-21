let _ =
  Printf.printf "draw lib called\n";
  let c = ref 0 in
  let continue = ref true in
  while !continue do
      try
        Utils.Draw.pin_dot (Printf.sprintf "/home/adriroot/Nextcloud/cours/mag/ter/factorisation-semigroupes/partition_algebra/img/diagram%i.dot" !c);
        incr c
      with Sys_error s -> (* Printf.printf "dernier fichier+1: %s\n" s; *)
        continue := false
  done
