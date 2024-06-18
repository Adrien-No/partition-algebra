(* Modifie les fichiers .dot représentant les diagrammes à afficher de telle sorte à corriger les erreurs typographiques de la bibliothèque ocamlgraph.graphviz provoquant des bugs (mauvaise génération des fichiers .dot) *)

let draw_diagram () =
  (* draw diagrams *)
  let c = ref 0 in
  let continue = ref true in
  while !continue do
      try
        Lib.Draw.pin_dot_and_typo (Sys.getcwd() ^ Printf.sprintf "/img/diagram%i.dot" !c);
        incr c
      with Sys_error _s -> (* Printf.printf "dernier fichier+1: %s\n" _s; *)
        continue := false
  done

let send_to_oeis file s =
  Printf.fprintf file "lookup %s\n" s

let sol = Lib.Toolbox.string_of_int_list

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

let seq algebra k_max gens =
  List.init k_max (fun i ->
    let module D = (val get_algebra algebra (i+1)) in
    D.generate gens |> List.length
    )

let _ =
  let planar = seq Okada 4 [B; P; Id] in
  Printf.printf "planar labelled : %s\n" (sol planar);
  (* List.map (fun x -> Lib.Maths.prime_decomp x |> List.length) planar |> sol |> Printf.printf "decomp : %s\n"; *)
  draw_diagram()
