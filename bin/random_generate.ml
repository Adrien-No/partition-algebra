let okada k gens =
  let module Okada = Lib.Labelled.Make(struct let k = k end) in
  (* Random.self_init(); *)
  let gens =
    let generate_generators f imax = if imax >= 0 then List.init imax Int.succ |> List.map f else [] in
    List.concat (List.map (fun (f, imax) -> generate_generators f imax) (List.map Okada.get_generator gens))
    |> List.map Lib.Labelled.Utils.sort |> Array.of_list
  in
  let ngens = Array.length gens in


  let random_gen() =
    gens.(Random.int (ngens))
  in
  let d = List.fold_left Okada.concat Okada.id (List.init ngens (fun _ -> random_gen())) in
  Okada.print d
