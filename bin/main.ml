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

let okada2() =
  let module Okada = Lib.Labelled.Make(struct let k = 2 end) in
  let planar = Okada.generate [B; P; Id] in
  List.iter Okada.print planar

let _ =
  (* let planar = *)
  (*   List.init 6 (fun k -> *)
  (*       let module Okada = Lib.Labelled.Make(struct let k = k end) in *)
  (*       let planar = Okada.generate [B; P; Id] in *)
  (*       planar) in *)
  (* let planar_seq = List.map List.length planar in *)
  (* Lib.Toolbox.string_of_int_list planar_seq |> Printf.printf "planar Okada: %s\n"; *)
  (* Lib.Toolbox.string_of_int_list (List.map (fun x -> Lib.Maths.prime_decomp x |> List.length ) planar_seq) |> Printf.printf "nb_decomp: %s\n"; *)
  (* okada2(); *)
  draw_diagram()

(* stabilisateurs *)
let _ =
  for k = 1 to 2 do
    let module Okada = Lib.Labelled.Make(struct let k = k end) in
    let planar = Okada.generate [B; P; Id] in
    List.map (fun d ->
        let rec stabilize d' acc =
          let new_d = d @ d' in
          if new_d = d' then (acc+1)
          else stabilize new_d (acc+1)
        in
        stabilize d 0
      ) planar
    |> Lib.Toolbox.string_of_int_list |> Printf.printf "k = %i stabilisateurs : %s" k
  done
