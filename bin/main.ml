(* Modifie les fichiers .dot représentant les diagrammes à afficher de telle sorte à corriger les erreurs typographiques de la bibliothèque ocamlgraph.graphviz provoquant des bugs (mauvaise génération des fichiers .dot) *)

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

module Okada2 = Lib.Labelled.Okada(struct let k = 2 end)
module D2 = Lib.Labelled.Make(Okada2)
open Lib.Diagram
let gens = [B; P]

(* on défini le nombre de composante comme le nb de sous-ensemble de taile > 1 *)
let nb_comp =
  List.fold_left (fun init -> function _, [] -> init | _ -> init+1 ) 0

let _ =
  (* D2.print_empty() *)
  let sg = D2.generate gens |> List.sort compare |> List.sort (fun x y -> compare (nb_comp y) (nb_comp x)) in
  (* List.iter (fun d -> D2.print_as_string d; print_newline()) sg; *)
  (* Printf.printf "size= %i\n" (List.length sg); *)
  List.iter D2.print sg


let _ =
  let planar_seq = seq Okada 4 gens in
  String.concat " "(planar_seq |> List.map string_of_int |> List.map (Printf.sprintf "%10s")) |> Printf.printf "%15s %s\n" "planar Okada :";
  String.concat " " (List.map (fun x -> Lib.Maths.prime_decomp x |> List.map string_of_int |> String.concat "x") planar_seq |> List.map (Printf.sprintf "%10s")) |> Printf.printf "%15s %s\n" "decomposition :";

  (* List.map (fun x -> Lib.Maths.prime_decomp x |> List.length) planar |> sol |> Printf.printf "decomp : %s\n"; *)
  Output.draw_diagram()
