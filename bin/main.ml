(* Modifie les fichiers .dot représentant les diagrammes à afficher de telle sorte à corriger les erreurs typographiques de la bibliothèque ocamlgraph.graphviz provoquant des bugs (mauvaise génération des fichiers .dot)*)

let _ =
  let c = ref 0 in
  let continue = ref true in
  while !continue do
      try
        Utils.Draw.pin_dot_and_typo (Sys.getcwd() ^ Printf.sprintf "/img/diagram%i.dot" !c);
        incr c
      with Sys_error s -> (* Printf.printf "dernier fichier+1: %s\n" s; *)
        continue := false
  done
