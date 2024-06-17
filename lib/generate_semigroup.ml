(* not so much used since the main code is on labelled_diagram.. we could maybe divided between theses two files *)

let generate_generators f imax = List.init imax Int.succ |> List.map f
let gg = generate_generators


(* on va essayer de forcer de partir de chacun des generateurs *) (* TODO perte de performances avec le type option ? *)
let make (elts : 'a list) (concat : 'a -> 'a -> 'a) (sort : 'a -> 'a) f : 'a list =
  let cache = Hashtbl.create (List.length elts) in
  let elts = List.map sort elts in
  let rec loop (d: 'a option) : unit =
    match d with
    | None -> List.iter (fun concated -> loop (Some concated)) elts
    | Some d ->
      if not (Hashtbl.mem cache d) then
      begin
        Hashtbl.replace cache (sort d) true;
        let nexts = List.map (concat d) elts in
        (* List.iter (fun d -> Hashtbl.add cache d ()) nexts; *)
        List.iter (fun concated -> loop (Some concated)) nexts
      end
  in
  loop None;
  Printf.printf "contenu de la table de hachage:\n";
  Hashtbl.iter (fun x y -> f x; print_newline()) cache;
  let l = Hashtbl.to_seq cache |> List.of_seq |> List.map fst |> List.sort_uniq compare in
  l
