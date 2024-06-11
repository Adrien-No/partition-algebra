open Utils

module P:Labelled_diagram.PARAM with type label = int and type node = int = struct
  type label = int
  type node = int
  let k = 4
  (* List.fold_left min max_int n'est pas bon car ne tiens pas compte de la "valeur absolue sur les noeuds"*)
  (** [law labels] computes the resulted label optained by applying a fixed law to [labels] 2by2. (labels are already in unconverted mode : in set [-k; k]\{0}) (but k>0)) *)
  let law = List.fold_left (fun x y -> min (abs x) (abs y)) k
  (* List.fold_left (fun x y -> Printf.printf "y=%i\n" y; if ((unconvert k x)|>abs) < ((unconvert k y)|>abs) then x else y) k (\* List.fold_left (fun x y -> if ((unconvert k x)|>abs) < ((unconvert k y)|>abs) then x else y) max_int *\) *)
  (** [init_label nodes] is [law (map Toolbox.externalize nodes)]. Indeed, nodes aren't yet converted.*)
  let init_label nodes = law (List.map (Toolbox.externalize k) nodes)

  let lab_to_string = string_of_int
  let node_to_string = string_of_int
end

module D = Labelled_diagram.Make(P)
open D
let _ =
  print (e 1 @ e 2);
  assert (D.is_okada_diagram (e 1 @ e 2))
  (* List.fold_left (fun d d' -> print d'; d@d') id [e 1; e 2; e 3] |> (fun d -> print_empty(); print d) *)
