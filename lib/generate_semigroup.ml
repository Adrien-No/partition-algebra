(* not so much used since the main code is on labelled_diagram.. we could maybe divided between theses two files *)

type t = Diagram.diagram

let sort_shapely (d: t) (d': t) =
  let open Diagram in
  let forget_lab d = List.map (function Unique i -> Unique i | Few (_, comp) -> Few (0, List.sort compare comp)) d |> List.sort compare in
  let d, d' = forget_lab d, forget_lab d' in
  compare d d'

let generate_generators f imax = List.init imax Int.succ |> List.map f
let gg = generate_generators

(* on va essayer de forcer de partir de chacun des generateurs *) (* TODO perte de performances avec le type option ? *)
let make1 (elts : 'a list) (concat : 'a -> 'a -> 'a) (normalize : 'a -> 'a) (to_string) : 'a list =
  let cache = Hashtbl.create (List.length elts) in
  (* let elts = List.map sort elts in *)
  let rec loop (d: 'a option) : unit =
    match d with
    | None -> List.iter (fun concated -> loop (Some concated)) elts
    | Some d ->
      let d = normalize d in
      if not (Hashtbl.mem cache ((* sort *) d)) then
        begin
          Hashtbl.replace cache ((* sort *) d) true;
          (* for i = 0 to 100 do *)
          let nexts = List.map (concat d) elts (* @ List.map (fun d' -> concat d' d) elts *) in
          (* etait commenté *)
          (* List.iter (fun d -> Hashtbl.add cache d false) nexts; *)
          (*  *)
          List.iter (fun concated -> loop (Some concated)) nexts
          (* done *)
        end
  in
  loop None;
  (* Printf.printf "contenu de la table de hachage:\n"; *)
  (* Hashtbl.iter (fun x y -> Printf.printf "%s\n" (to_string x); print_newline()) cache; *)
  let l = Hashtbl.to_seq cache |> List.of_seq |> List.map fst |> List.sort sort_shapely in (* /!\ pas mettre de sort_uniq sinon on garde que les "classes diagrammes" (egaux à isomorphismes près) *)
  (* Printf.printf "taille de l : %i\n" (List.length l); *)
  l

let make2 (gens: t list) (concat : t -> t -> t) (sort : t -> t) : t list =
  let cache = Hashtbl.create (List.length gens) in
  List.iter (fun gen -> Hashtbl.add cache gen ()) gens;

  let rec loop ((* elts: t list *)) : unit =
    (* authentics are the "canonical" of their equivalent class, two  *)
    let news = ref [] in
    Hashtbl.iter (fun d _ ->
        List.iter (fun gen ->
            let newd = concat d gen in
            if Hashtbl.mem cache newd then
              ()
            else begin
              Hashtbl.add cache newd ();
              news := newd :: !news
            end
      ) gens;
      ) cache;
    if !news != [] then
      begin
        List.iter (fun d -> Hashtbl.add cache d ()) (List.sort_uniq compare !news);
        loop()
      end
  in
  loop();
  Hashtbl.to_seq cache |> List.of_seq |> List.map fst |> List.sort_uniq compare (* sort pour meilleur affichage *)

(* (\* on va aussi afficher une decomp. des elements *\) *)
(* let make3 gens concat = *)
(*   let cache = Hashtbl.of_seq (List.to_seq) *)

let profmax = 6
let make4 gens concat id =
  let cache = Hashtbl.create (List.length gens) in
  let rec loop d prof =
    (* if Hashtbl.mem cache d then () *)
    (* else  *)if prof = 0 then ()
    else begin
      Hashtbl.add cache d ();
      List.iter (fun d' ->
          loop (concat d d') (prof-1)
        ) gens;
      end
   in
   loop id profmax;
   (* Printf.printf "saluut\n"; *)
   let res = Hashtbl.to_seq cache |> List.of_seq |> List.map fst |> List.sort_uniq compare in
   (* assert(Hashtbl.length cache = List.length res); *)
   res

let make x y _ _ = make1 x y
