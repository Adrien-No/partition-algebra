open Utils.Diagram

let k = 11
module Partition = Diagram (struct let k = k end : sig val k : int end)

open Partition

let print_concat d d' =
  print d;
  print d';
  print(concat d d')

(* let symmetric_jump len = *)
(*   List.fold_left @ id (List.init ) *)
(*     s 1 @ s 3 @ e 2 @ s 1 @ s 3 *)

let jump2 i =
  s i @ s (i+2) @ e (i+1) @ s i @ s (i+2)

(* let jump1 i = *)

(* let long_swap *)

exception Error of Partition.t * Partition.t
let tester() =
  let errorer x y =
  if x === y then () (* l'égalité correspond à celle dans la partition *)
  else raise (Error (x, y))
  in
  try
    let a =
      let premiere_figure = b 1 @ p 1 @ l 2 @ b 4 in
      let troisieme_figure = s 6 @ (b 4 @ b 5 @ p 4 @ p 5) @ (s 6 @ s 5) @ s 6 @ (b 3 @ b 4) @ p 3 @ p 4 @ p 5 in
      let quatrieme_figure = s 8 @ s 7 @ b 9 @ p 9 @ p 10 @ p 11 @ b 8 @ b 10 @ s 9 in
      troisieme_figure @ premiere_figure @ quatrieme_figure
    and
      b =
      let zero = s 10 @ s 2 in (* zero is used to highlight permutations we choose to add at the beginning but thought later *)
      let fst = (s 1 @ s 2 @ s 3) @ (b 3 @ e 4) @ p 4 in
      let snd = p 2 @ p 3 @ b 2 in
      let trd = b 6 @ p 7 @ b 4 @ s 5 in
      zero @ fst @ snd @ trd @
      b 9 @ p 9 @ p 10 @ p 11 @ b 8 @ b 9 @ s 10
    and
      c = ([[17]; (* pas besoin de traduire car déjà traduit *)
                   [16; 14];
                   [13; 12];
                   [21; 19; 18];
                   [10];
                   [9; 7];
                   [15; 8; 6; 4; 3];
                   [5; 2];
                   [11; 1; 0]])
    in
    errorer (concat a b) c
  with Error (d, d') -> print d; print d'; Printf.printf "[ERROR] generator_tests: not equal" (* we don't really raise so diagrams can be printed *)

(* let generate_symmetric_group k = *)
(*   let cache = Hashtbl.create  *)

let _ =
  tester();
  print id
