(* not so much used since the main code is on labelled_diagram.. we could maybe divided between theses two files *)

let generate_generators f imax = List.init imax Int.succ |> List.map f
let gg = generate_generators


(* on va essayer de forcer de partir de chacun des generateurs *) (* TODO perte de performances avec le type option ? *)
let make (elts : 'a list) (concat : 'a -> 'a -> 'a) (sort : 'a -> 'a) : 'a list =

  let cache = Hashtbl.create (List.length elts) in
  (* let elts = List.map sort elts in *)
  let rec loop (d: 'a option) : unit =
    match d with
    | None -> List.iter (fun concated -> loop (Some concated)) elts
    | Some d ->
      (* match Hashtbl.find_opt cache d with *)
      (* | None -> Hashtbl.add cache d 1; *)
      (*   let nexts = List.map (concat d) elts @ List.map (fun d' -> concat d' d) elts in *)
      (*   List.iter (fun concated -> loop (Some concated)) nexts *)
      (* | Some n when n < 10 -> Hashtbl.replace cache d (n+1); *)
      (*   let nexts = List.map (concat d) elts @ List.map (fun d' -> concat d' d) elts in *)
      (*   List.iter (fun concated -> loop (Some concated)) nexts *)
      (* | _ -> () *)
      if not (Hashtbl.mem cache d) then begin
        Hashtbl.replace cache d true;
        let nexts = List.map (concat d) elts @ List.map (fun d' -> concat d' d) elts in
        List.iter (fun concated -> loop (Some concated)) nexts
      end
  in
  loop None;
  let l = Hashtbl.to_seq cache |> List.of_seq |> List.map fst in
  assert (List.sort compare l = List.sort_uniq compare l);
  l

type d = Diagram.diagram_t
let cayley (generators: d list) (concat: d -> d -> d) =
  let open Hashtbl in

  let generator_of_int = Array.of_list generators in

  (* [0] build the right-cayley graph of the sg generated by generators with a dfs *)
  let ngens = List.length generators in (* NOTE we could use Vectors instead *)
  let int_of_diagram = create ngens
  and diagram_of_int = create ngens
  and g : (int, (int, int) t) t = create ngens in (* (find (find g src) dst) gives the index of the generator that produce dst from src (with a right concatenation) *)
  let init_d (d: d) : int =
    match find_opt int_of_diagram d with
      None ->
      let n = length int_of_diagram in
      add int_of_diagram d n;
      add diagram_of_int n d;
      n
    (* add g n (Hashtbl.create 1) (\* n isn't already binded in g *\) *) (* we adds n to g later *)
    | Some n -> n
  in
  let rec loop (i_src: int) : unit = (* dfs *)
    if i_src = -1 then
      List.iter (fun d -> loop (init_d d)) generators
    else if not (Hashtbl.mem g i_src) then begin
      add g i_src (create 1);
      List.iteri (fun i_gen gen ->
          (* we wants to compute dst and add (lab, dst) to the graph where lab is the index of the generator who send src to dst *)
          let dst = concat (find diagram_of_int i_src) gen in
          let i_dst = init_d dst in
          add (find g i_src) i_dst i_gen; (* i_gen is the label of edge (i_src, i_dst) *)
          loop (i_dst) (* we keep exploring from this new node *)
        ) generators
    end
  in
  loop (-1)

  (* [1] ajouter un module "cayley graphe" dans draw, (renommer l'ancien de Dot vers Diagram), creer les .png representant les diagrammes et bien les nommer, puis enfin construire un .png representant le grpahe de cayley en utilisant ces png*)
    (* tester cayley. *)
