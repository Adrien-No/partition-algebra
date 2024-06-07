Printexc.record_backtrace true

open Utils.Labelled_diagram

(** [internalize k i] converts i in a diagram of size 2k from set [-k; k]\{0} to set [0; 2k-1].
    It correspond to the external (resp internal) numerotation of diagram vertices. *)
let internalize k x =
  if x > 0 then x - 1
  else k - x - 1

(** [externalize k i] converts i in a diagram of size 2k from set [0; 2k-1] to set [-k; k]\{0}.
    It correspond to the internal (resp external) numerotation of diagram vertices. *)
let externalize k i =
  if i < k then (i+1)
  else -(i-k+1)

module P:PARAM with type label = int and type node = int = struct
  type label = int
  type node = int
  let k = 3
  (* List.fold_left min max_int n'est pas bon car ne tiens pas compte de la "valeur absolue sur les noeuds"*)
  (** [law labels] computes the resulted label optained by applying a fixed law to [labels] 2by2. (labels are already in unconverted mode : in set [-k; k]\{0}) (but k>0)) *)
  let law = List.fold_left (fun x y -> min (abs x) (abs y)) k
    (* List.fold_left (fun x y -> Printf.printf "y=%i\n" y; if ((unconvert k x)|>abs) < ((unconvert k y)|>abs) then x else y) k (\* List.fold_left (fun x y -> if ((unconvert k x)|>abs) < ((unconvert k y)|>abs) then x else y) max_int *\) *)
  (** [init_label nodes] is [law (map externalize nodes)]. Indeed, nodes aren't yet converted.*)
  let init_label nodes = law (List.map (externalize k) nodes)

  let lab_to_string = string_of_int
  let node_to_string = string_of_int
end

module Partition = Make (P:PARAM with type label = int)
open Partition

exception Error of Partition.t * Partition.t
let errorer x y =
  if x === y then () (* l'égalité correspond à celle dans la partition *)
  else raise (Error (x, y))

let test_adder () =
  let a = id in
  let b = s 1 in
  let c : Partition.t = unsafe_create [(1, [1; -2]); (1, [2; -1]); (3, [3; -3])] (* |> List.map (fun (label, l) -> convert P.k label, l) *) in
  print a;
  print b;
  print (a @ b);
  print c

let generators_extended() =
  try
    (* #### début de l'écriture des tests #### *)
    Printf.printf "test 0\n";errorer (s 1) (unsafe_create [(1, [1; -2]); (1, [2; -1]); (3, [3; -3])]);
    Printf.printf "test 1\n";errorer (s 1 @ s 1) (unsafe_create [(1, [1; -1]); (1, [2; -2]); (3, [3; -3])]); (* a case where a label is reduced *)
    Printf.printf "test 2\n";errorer (p 1) (of_ill [[1]; [-1]; [2; -2]; [3; -3]]);
    Printf.printf "test 3\n";errorer (s 1 @ id) (unsafe_create [(1, [1; -2]); (1, [2; -1]); (3, [3; -3])]);
    Printf.printf "test 4\n";errorer (id @ s 1) (of_ill [[1; -2]; [2; -1]; [3; -3]]);
    Printf.printf "test 5\n";errorer (l 1) (unsafe_create [[1; -2]; [3; -3]; [2]; [-1]]);
    (* errorer (l 2) (of_ill [[1; -1]; [2; -3]; [-2]; [3]]); *)
    (* errorer (e 1) (of_ill [[1; 2]; [-1; -2]; [3; -3]]); *)
    (* errorer (e 2) (of_ill [[1; -1]; [2; 3]; [-2; -3]]); *)
    (* errorer (r 1) (of_ill [[-1; 2]; [3; -3]; [1]; [-2]]); *)
    (* errorer (r 2) (of_ill [[1; -1]; [-2; 3]; [2]; [-3]]) *)

    (* #### fin de l'écriture des tests #### *)
  with Error (d, d') -> print_as_string d; print_as_string d'; print d; print d'; Printf.printf "[ERROR] generator_tests: not equal" (* we don't really raise so diagrams can be printed *)

let _ =
  (* test_adder(); *)
  generators_extended();
  print_empty()
  (* p 1 @ p 2 |> print *)
