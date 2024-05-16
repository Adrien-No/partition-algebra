open Utils.Uf_persistant

let _ =
 let uf = Uf.create 8 in
 let uf = Uf.union uf (7) 1 in
 let _uf = Uf.union uf 1 4 in ()
 (* Printf.printf "cl de 1 :%i\n" (Uf.find uf 1); *)
 (* Printf.printf "cl de 4 :%i\n" (Uf.find uf 4); *)
 (* Printf.printf "cl de 5 :%i\n" (Uf.find uf (5)) *)
