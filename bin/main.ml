open Lib.Diagram

(* (\* Modifie les fichiers .dot représentant les diagrammes à afficher de telle sorte à corriger les erreurs typographiques de la bibliothèque ocamlgraph.graphviz provoquant des bugs (mauvaise génération des fichiers .dot) *\) *)

let draw_diagram () =
  (* draw diagrams *)
  let c = ref 0 in
  let continue = ref true in
  while !continue do
    try
      Lib.Draw.pin_dot_and_typo (Sys.getcwd() ^ Printf.sprintf "/img/diagram%i.dot" !c);
      incr c
    with Sys_error _s -> (* Printf.printf "dernier fichier+1: %s\n" s; *)
      continue := false
  done

(* let _ = *)
(*   let planar = generate Okada [B; P; Id] 4 in *)
(*   Printf.printf "planar labelled : %s\n" (sol planar); List.map (fun x -> Lib.Maths.prime_decomp x |> List.length) planar |> sol |> Printf.printf "decomp : %s\n"; *)
(*   let module Okada2 = (val k_okada 2) in *)
(*   let len, elts = Lib.Generate_semigroup.make Okada2.concat (List.map Okada2.get_generator [B; P; Id]) in *)
(*   (\* List.iter (fun d -> Okada2.print_as_string d; print_newline()) elts; *\) *)
(*   List.iter Okada2.print elts; *)

let nb_composante d =
  let open Lib.Labelled in
  List.fold_left (fun init -> function Unique _ -> init+1 | Few _ -> init+1 ) 0 d

(* let okada2() = *)
(*   let module Okada = Lib.Labelled.Make(struct let k = 2 end) in *)
(*   let open Lib.Diagram in *)
(*   let gens = [B; P] in *)
(*   let _gens_d = *)
(*     let generate_generators f imax = if imax >= 0 then List.init imax Int.succ |> List.map f else [] in *)
(*     List.concat (List.map (fun (f, imax) -> generate_generators f imax) (List.map Okada.get_generator gens)) *)
(*     |> List.map Lib.Labelled.Utils.sort *)
(*   in *)
(*   (\* List.iter Okada.print gens_d; Okada.print [] *\) *)
(*   let planar = Okada.generate gens |> List.sort (fun x y -> compare (nb_composante y) (nb_composante x)) in *)
(*   List.iter Okada.print planar *)

(* let _ = *)
(*   let planar = *)
(*     List.init 4 (fun k -> *)
(*         let module Okada = Lib.Labelled.Make(struct let k = k end) in *)
(*         let planar = Okada.generate [B; P; Id] in *)
(*         planar) in *)
(*   let planar_seq = List.map List.length planar in *)
(*   String.concat " "(planar_seq |> List.map string_of_int |> List.map (Printf.sprintf "%10s")) |> Printf.printf "%15s %s\n" "planar Okada :"; *)
(*   String.concat " " (List.map (fun x -> Lib.Maths.prime_decomp x |> List.map string_of_int |> String.concat "x") planar_seq |> List.map (Printf.sprintf "%10s")) |> Printf.printf "%15s %s\n" "decomposition :"; *)

(* let _ = *)
(*   Random_generate.okada 5 [E]; *)
(*   draw_diagram() *)

let g_temperleylieb = [E; Id]
let g_planar = [P; B; Id]

let _ =
  (* let module D = Lib.Labelled.Make(struct let k = 5 end) in *)
  (* let open D in *)
  (* print (e 2 @ e 1 @ e 3 @ e 2 @ e 4 @ e 3); *)
  draw_diagram()
