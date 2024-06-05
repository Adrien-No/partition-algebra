open Utils.Labelled_diagram

module P:PARAM = struct
  type label = int
  type node = int
  let k = 3
  let law = List.fold_left min max_int
  let init_label = law

  let lab_to_string = string_of_int
  let node_to_string = string_of_int
end

module Partition = Make (P:PARAM)
open Partition

exception Error of Partition.t * Partition.t
let errorer x y =
  if x === y then () (* l'égalité correspond à celle dans la partition *)
  else raise (Error (x, y))

let generators_extended() =
  try
    (* #### début de l'écriture des tests #### *)
    errorer (l 1) (of_ill [[1; -2]; [3; -3]; [2]; [-1]]);
    errorer (l 2) (of_ill [[1; -1]; [2; -3]; [-2]; [3]]);
    errorer (e 1) (of_ill [[1; 2]; [-1; -2]; [3; -3]]);
    errorer (e 2) (of_ill [[1; -1]; [2; 3]; [-2; -3]]);
    errorer (r 1) (of_ill [[-1; 2]; [3; -3]; [1]; [-2]]);
    errorer (r 2) (of_ill [[1; -1]; [-2; 3]; [2]; [-3]]);

    (* #### fin de l'écriture des tests #### *)
  with Error (d, d') -> print_as_string d; print_as_string d'; print d; print d'; Printf.printf "[ERROR] generator_tests: not equal" (* we don't really raise so diagrams can be printed *)

let _ =
  generators_extended();
  p 1 @ p 2 |> print
