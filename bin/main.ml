(* Modifie les fichiers .dot représentant les diagrammes à afficher de telle sorte à corriger les erreurs typographiques de la bibliothèque ocamlgraph.graphviz provoquant des bugs (mauvaise génération des fichiers .dot) *)

let draw_diagram () =
  (* draw diagrams *)
  let c = ref 0 in
  let continue = ref true in
  while !continue do
      try
        Lib.Draw.pin_dot_and_typo (Sys.getcwd() ^ Printf.sprintf "/img/diagram%i.dot" !c);
        incr c
      with Sys_error _s -> Printf.printf "dernier fichier+1: %s\n" s;
        continue := false
  done

let send_to_oeis file s =
  Printf.fprintf file "lookup %s\n" s

let sol = Lib.Toolbox.string_of_int_list

let cardinal_of_semigroup (module D: Lib.Diagram.t) (gens: Lib.Diagram.generators list) =
  fst (Lib.Generate_semigroup.make
         (List.map D.get_generator gens|> List.map (fun (f,imax) -> Lib__Generate_semigroup.gg f imax)
         D.concat D.sort)

let k_okada k =
  let module Okada = Lib.Labelled.Okada (struct let k = k end) in
  let module D = Lib.Labelled.Make(Okada) in
  (module D : Lib.Diagram.t)

let k_partition k =
  let module D = Lib.Unlabelled.Make(struct let k = k end) in
  (module D : Lib.Diagram.t)

type algebra = Okada | Partition
let get_algebra = function
    Okada -> k_okada
  | Partition -> k_partition

  let elt1 = D2.of_ill [[1; -1]; [2; -2]]
  and elt2 = D2.of_ill [[2; -2]; [1; -1]] in
  D2.print_as_string elt1;
  D2.print_as_string elt2;
  assert (elt1 = elt2)
let generate choice generators len =
  List.init len Int.succ
  |> List.map (fun k -> cardinal_of_semigroup (get_algebra choice k) generators)

let _ =
  let planar = generate Okada [B; P; Id] 4 in
  Printf.printf "planar labelled : %s\n" (sol planar); List.map (fun x -> Lib.Maths.prime_decomp x |> List.length) planar |> sol |> Printf.printf "decomp : %s\n";
  let module Okada2 = (val k_okada 2) in
  let len, elts = Lib.Generate_semigroup.make Okada2.concat (List.map Okada2.get_generator [B; P; Id]) in
  (* List.iter (fun d -> Okada2.print_as_string d; print_newline()) elts; *)
  List.iter Okada2.print elts;

let _ = draw_diagram()
