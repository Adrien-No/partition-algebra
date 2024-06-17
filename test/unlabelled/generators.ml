(* Printexc.record_backtrace true *)

(* open Lib.Toolbox *)
(* open Lib.Unlabelled_diagram *)

(* let k = 3 *)
(* module Partition = Make (struct let k = k end : sig val k : int end) *)
(* open Partition *)

(* exception Error of Partition.t * Partition.t *)
(* let errorer x y = *)
(*   if x === y then () (\* l'égalité correspond à celle dans la partition *\) *)
(*   else raise (Error (x, y)) *)

(* let generators_extended() = *)
(*   try *)
(*     (\* #### début de l'écriture des tests #### *\) *)
(*     errorer (l 1) (of_ill [[1; -2]; [3; -3]; [2]; [-1]]); *)
(*     errorer (l 2) (of_ill [[1; -1]; [2; -3]; [-2]; [3]]); *)
(*     errorer (e 1) (of_ill [[1; 2]; [-1; -2]; [3; -3]]); *)
(*     errorer (e 2) (of_ill [[1; -1]; [2; 3]; [-2; -3]]); *)
(*     errorer (r 1) (of_ill [[-1; 2]; [3; -3]; [1]; [-2]]); *)
(*     errorer (r 2) (of_ill [[1; -1]; [-2; 3]; [2]; [-3]]); *)

(*     (\* #### fin de l'écriture des tests #### *\) *)
(*   with Error (d, d') -> print d; print d'; Printf.printf "[ERROR] generator_tests: not equal" (\* we don't really raise so diagrams can be printed *\) *)

(* let _ = *)
(*   generators_extended(); *)
(*   p 1 @ p 2 |> print *)
