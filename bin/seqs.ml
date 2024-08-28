(* let sol = Lib.Toolbox.string_of_int_list *)

(* let cardinal_of_semigroup (module D: Lib.Diagram.t) (gens: Lib.Diagram.generators list) = *)
(*   Lib.Generate_semigroup.make D.concat (List.map D.get_generator gens) *)

(* let k_okada k = *)
(*   let module Okada = Lib.Labelled.Okada (struct let k = k end) in *)
(*   let module D = Lib.Labelled.Make(Okada) in *)
(*   (module D : Lib.Diagram.t) *)

(* let k_partition k = *)
(*   let module D = Lib.Unlabelled.Make(struct let k = k end) in *)
(*   (module D : Lib.Diagram.t) *)

(* type algebra = Okada | Partition *)
(* let get_algebra = function *)
(*     Okada -> k_okada *)
(*   | Partition -> k_partition *)

(* let generate choice generators len = *)
(*   List.init len Int.succ *)
(*   |> List.map (fun k -> cardinal_of_semigroup (get_algebra choice k) generators) *)
(*   (\* |> Lib.Toolbox.string_of_int_list *\) *)
