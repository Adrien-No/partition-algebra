Printexc.record_backtrace true

let k = 3
module D = Lib.Labelled.Make(struct let k = k end)
open D
open Lib.Labelled

let external_lab_create l =
  List.map (function
      | label, [] -> failwith "wrongly created"
      | _label, [node] -> Unique (Lib.Toolbox.internalize k node)
      | label, nodes -> Few(label, List.map (Lib.Toolbox.internalize k) nodes)
  ) l |> Utils.sort

let external_unlab_create l =
  List.map (function
    | [] -> failwith "wrongly created"
    | [node] -> Unique (Lib.Toolbox.internalize k node)
    | nodes ->
      Few(List.fold_left min k (List.map abs nodes), List.map (Lib.Toolbox.internalize k) nodes)) l
  |> Utils.sort

exception Error of int * D.t * D.t
let errorer ntest x y =
  if x === y then () (* l'égalité correspond à celle dans la partition *)
  else raise (Error (ntest+38, x, y))

let test_adder () =
  let a = e 1 in
  let b = e 2 in
  let c' = external_lab_create [(1, [-1; 3]); (1, [1; 2]); (2, [-2; -3])] in
  print a;
  print b;
  print (a @ b);
  print c'

let generators_extended() =
  try
    (* #### début de l'écriture des tests #### *)
    errorer 1 (s 1) (external_lab_create [(1, [1; -2]); (1, [2; -1]); (3, [3; -3])]);
    errorer 2 (s 1 @ s 1) (external_lab_create [(1, [1; -1]); (1, [2; -2]); (3, [3; -3])]); (* a case where a label is reduced *)
    errorer 3 (p 1) (external_unlab_create [[1]; [-1]; [2; -2]; [3; -3]]);
    errorer 4 (s 1 @ id) (external_lab_create [(1, [1; -2]); (1, [2; -1]); (3, [3; -3])]);
    errorer 5 (id @ s 1) (external_unlab_create [[1; -2]; [2; -1]; [3; -3]]);
    errorer 6 (l 1) (external_unlab_create [[1; -2]; [3; -3]; [2]; [-1]]);
    errorer 7 (l 2) (external_unlab_create [[1; -1]; [2; -3]; [-2]; [3]]);
    errorer 8 (e 1) (external_unlab_create [[1; 2]; [-1; -2]; [3; -3]]);
    errorer 9 (e 2) (external_unlab_create [[1; -1]; [2; 3]; [-2; -3]]);
    errorer 10(r 1) (external_unlab_create [[-1; 2]; [3; -3]; [1]; [-2]]);
    errorer 11(r 2) (external_unlab_create [[1; -1]; [-2; 3]; [2]; [-3]]);
    errorer 12(l 1 @ r 1) (p 2);
    errorer 13(r 1 @ l 1) (external_lab_create [(1, [1]); (1, [-1]); (1, [2; -2]); (3, [3; -3])]);
    errorer 14(e 1 @ e 2) (external_lab_create [(1, [-1; 3]); (1, [1; 2]); (2, [-2; -3])])
    (* #### fin de l'écriture des tests #### *)
  with Error (line, d, d') ->
    Printf.printf "%s\n" (to_string d);
    Printf.printf "%s\n" (to_string d');
    print d; print d';
    Printf.printf "[ERROR LINE %i] generator_tests: not equal" line (* we don't really raise so diagrams can be printed *)

let _ =
  (* test_adder(); *)
  generators_extended()
  (* p 1 @ p 2 |> print *)
