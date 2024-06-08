open Utils

let _ =
  for i = 1 to 4 do
    let module P:Labelled_diagram.PARAM with type label = int and type node = int = struct
      type label = int
      type node = int
      let k = i
      (* List.fold_left min max_int n'est pas bon car ne tiens pas compte de la "valeur absolue sur les noeuds"*)
      (** [law labels] computes the resulted label optained by applying a fixed law to [labels] 2by2. (labels are already in unconverted mode : in set [-k; k]\{0}) (but k>0)) *)
      let law = List.fold_left (fun x y -> min (abs x) (abs y)) k
      (* List.fold_left (fun x y -> Printf.printf "y=%i\n" y; if ((unconvert k x)|>abs) < ((unconvert k y)|>abs) then x else y) k (\* List.fold_left (fun x y -> if ((unconvert k x)|>abs) < ((unconvert k y)|>abs) then x else y) max_int *\) *)
      (** [init_label nodes] is [law (map Toolbox.externalize nodes)]. Indeed, nodes aren't yet converted.*)
      let init_label nodes = law (List.map (Toolbox.externalize k) nodes)

      let lab_to_string = string_of_int
      let node_to_string = string_of_int
    end in

    let module D = Labelled_diagram.Make(P) in
    let card = Generate_semigroup.labelled (module D) [(D.l, P.k-1); (D.r, P.k-1); (* (D.r, P.k-1) *)] in
    D.print_empty();
    Printf.printf "%i\n" card
  done;
