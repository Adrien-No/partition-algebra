(* let _ = *)
(* for k = 0 to 5 do *)
(*   let module D = Lib.Unlabelled.Make(struct let k = k end) in *)
(*   let open D in *)
(*   for i = 0 to k-1 do *)
(*     assert (s i === (s i @ s i)); *)
(*     assert (p i @ p i === p i) *)

(*   done; *)
(*   for i = 0 to k-2 do *)
(*     assert (b i @ b i === b i); *)
(*     assert (p i @ b i @ p i === p i); *)
(*     assert (e i === b i @ p i @ p (i+1) @ b i); *)
(*     assert (l i === s i @ p i); *)
(*     assert (r i === p i @ s i) *)
(*   done; *)
(*   for i = 0 to k-3 do *)
(*     assert (s i @ s (i+1) @ s i === s (i+1) @ s i @ s (i+1)); *)
(*   done; *)
(* done *)
