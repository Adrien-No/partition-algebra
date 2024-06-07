(* NOTE generators e, l and r are "MACROS" i.e we labelize them as concat of generators s, p and b. *)
(* e.g : with the generator l, cl [2] will have label 1 bc l 1 = s 1 @ p 1*)

(* auxiliar functions on Diagrams, used by functor Make *)
module Utils = struct
  type ('a, 'b) t = ('a * 'b list) list

  let iter f =
    List.iter (fun (_lab, l) -> List.iter f l)

  let map f =
    List.map (fun (lab, l) -> lab, List.map f l)

  let map_all f =
    List.map (fun (lab, l) -> f lab, List.map f l)
  (* let map_labels f = *)
  (*   List.map (fun (lab, l) -> f lab, l) *)

  let fold f init =
    List.fold_left (fun init (_lab, l) -> List.fold_left f init l) init

  let filter f l =
    List.map (fun (lab, l) -> lab, List.filter f l) l |> List.filter (fun (_,l) -> l <>[])

  let filter_map f l =
    map f l
    |> filter ((<>)None)
    |> map Option.get

  let sort l =
    List.map (fun (lab, l) -> lab, List.fast_sort compare l) l |> List.fast_sort compare (* un ordre total sur les diagrammes étiquetés *)

  let print (lab_to_string : 'a -> string) (node_to_string: 'b -> string) l =
  let l = List.map (fun (lab, l) -> ("(" ^ lab_to_string lab ^ ") [" ^ (l |> List.map node_to_string |> String.concat "; ") ^ "]")) l in
  "[" ^ (String.concat ";\n" l) ^ "]\n" |> Printf.printf "%s"

end

module type t = sig
  type t

  val of_ill : int list list -> t
  val unsafe_create : t -> t
  val id : t

  val s : int -> t
  val p : int -> t
  val b : int -> t
  val e : int -> t
  val l : int -> t
  val r : int -> t

  val concat : t -> t -> t
  val (@) : t -> t -> t
  val (===) : t -> t -> bool
  val print : t -> unit
  val print_as_string : t -> unit

  val print_empty : unit -> unit
end

module type PARAM = sig
  type label
  type node = int
  val k : int
  val law : label list -> label (* law is the composition law on labels used to determine the new label after concat *)
  val init_label : node list -> label (* used to init labels of cls of a generator *)

  val lab_to_string : label -> string
  val node_to_string : node -> string
end

module Make (P: PARAM with type label = int ) : t with type t = (P.label * P.node list) list = struct
  (* En interne, les diagrammes sont numérotés *)
  (* de 0 à k-1 (en haut, de gauche à droite) *)
  (* puis de k à 2k-1 (en bas, de gauche à droite) *)
  (* index goes from 0 to P.k-1 and P.k*2-1 to P.k *)

  (* NOTE par contre, puisque les labels sont en valeur absolue, on peut directement numéroter les arêtes de 1 à k ! *)

  (* Les générateurs sont numérotés en externe de 1 à k *)
  type t = cl list (* type of a labelled diagram *)
  and cl = P.label * P.node list (* type of a equiv class of a diagram *)

  (* (\** unsafe creation from \[-k;k\]\{0} to \[0; 2*k-1\]*\) *)
  (* let of_iil_l iil_l = *)
  (*   Utils.map *)
  (*   Toolbox.ll_map (fun (_, iil) -> {Toolbox.convert P.k iil}) iil_l |> Toolbox.ll_sort *)

  (* creates a labelled diagram from an unlabelled one with PARAM.init_label *)
  let of_unlabelled (f: int list -> P.label) = List.map (fun cl -> f cl, cl)

  let of_ill ill = Toolbox.ll_map (Toolbox.internalize P.k) ill |> of_unlabelled P.init_label |> Utils.sort

  let unsafe_create (d : t) : t = Utils.map (Toolbox.internalize P.k) d (* labels has already been converted (bc here we have generality for label type, but in example it's an int) *)

  let id : t =
    let unlabelled = List.init P.k (fun i -> ([i; P.k+i])) in
    unlabelled |> of_unlabelled P.init_label |> Utils.sort

  let range_test (i: int) (i_min: int) (i_max: int) : unit =
    if i < i_min || i > i_max then failwith (Printf.sprintf "[range_test_error] %i not in [%i..%i]" i i_min i_max)

  let check_diagram (d: t) : unit =
    let h = Hashtbl.create (P.k*2) in
    let range_test i = range_test i 0 (P.k*2-1) in
    Utils.iter (fun i -> range_test i; Hashtbl.add h i ()) d;
    if List.for_all Fun.id (List.init (P.k*2) (Hashtbl.mem h))
      && d = Utils.sort d
    then ()
    else begin
        Printf.printf "[ERROR] invariants not maintained\n";
        Utils.print P.lab_to_string P.node_to_string d;
        failwith "[error]" end

  let s i = (* NOTE opti : faire des générateurs pour i dans la range test, puis les appeler sans les reconstruire *)
    range_test i 1 (P.k-1);
    let unlabelled =
    let i = i-1 in
    List.init P.k (
      function
      | j when j = i   -> [j  ; P.k+j+1]
      | j when j = i+1 -> [j; P.k+j-1]
      | j ->              [j; P.k+j])
    in
    unlabelled |> of_unlabelled P.init_label |> Utils.sort

  let p i =
    range_test i 1 P.k;
    let unlabelled =
      let i = i-1 in
      let rec loop (acc: int list list) =
        function
        | j when j = P.k -> acc
        | j when j = i -> loop ([j]::[(P.k+j)]::acc) (j+1)
        | j -> loop ([j; P.k+j]::acc) (j+1)
      in
      loop [] 0
    in
    unlabelled |> of_unlabelled P.init_label |> Utils.sort

  let b i =
    range_test i 1 (P.k-1);
    let unlabelled =
      let i = i-1 in
      let rec loop (acc: int list list) =
        function
        | j when j = P.k -> acc
        | j when j = i -> loop ([i; i+1; P.k+i; P.k+i+1]::acc) (j+1)
        | j when j = i+1 -> loop acc (j+1) (* déjà géré au cas précédent *)
        | i -> loop ([i; P.k+i]::acc) (i+1)
      in
      loop [] 0
    in
    unlabelled |> of_unlabelled P.init_label |> Utils.sort

  let to_graph (diagram: t) =
    let open Draw in
    if diagram = [] then G.empty (* hack un peu moche pour afficher au besoin un espacement entre des diagrams *)
    else
      let g = (* G.empty *) (* test avec seulement les sommets utilisés *)
      List.fold_left
        G.add_vertex
        G.empty
        (List.init (P.k*2) Fun.id)(* ((List.init P.k Int.succ) @ (List.init P.k (fun i -> -(i+1)))) *)
      in
      (* Printf.printf "sommets du graphe: \n"; *)

      let rec loop_diagram (d: t) (g: Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Draw.Node)(Draw.Edge).t)  =
        match d with
          [] -> g
        | (label, cl)::cls ->
          let rec loop_cl prev_elem (cl : P.node list) g =
            match prev_elem with
            | None ->
              begin
                match cl with
                | [] -> g
                | elem::cl -> loop_cl (Some elem) cl g
              end
            | Some prev_elem ->
            match cl with
            | [] -> g
            | [elem] ->
              G.add_edge_e g (G.E.create prev_elem (P.lab_to_string label) elem) (* pas d'appel récursif car on a fini la liste *)
            | elem::elems ->
              let new_g = G.add_edge g prev_elem elem in (* d'après le match-case précédent, il y a encore au moins un élement dans elems *)
              loop_cl (Some elem) elems new_g
          in
          let new_g = loop_cl None cl g in
          loop_diagram cls new_g
      in
      loop_diagram diagram g

  let diagram_counter = ref 0
  let print (diagram: t) =
    let g = to_graph diagram in
    let file = open_out (Sys.getcwd() ^ "/../../../../img/diagram"^string_of_int !diagram_counter ^".dot") in
    incr diagram_counter;
    Draw.dot_as_graph file g P.k

  let print_empty () = print []

  let print_as_string d = Utils.print P.lab_to_string P.node_to_string ((* Utils.map_all (Toolbox.unconvert P.k) *) d)

  open Uf_persistant

  (** [ill_of_uf uf k] Queries the [uf] structure with integer from 0 to 2[k]-1, to rebuild the partition of [0; 2k-1] as an int list list *)
  let ill_of_uf uf n =
    (* n is the size of the uf (oftenly, n = 2*k) *)
    let canonical_index = Array.make n None in (* NOTE opti : take n = max |a| + |b| *)
    let dyna_res_arr = Array.make n [] in
    let len_res = ref 0 in
    for i = 0 to n-1 do
      let rpz = Uf.find uf i in
      let rpz_index_in_res =
        match canonical_index.(rpz) with
        | None ->
          (* new canonical found *)
          canonical_index.(rpz) <- Some (!len_res);
          incr len_res;
          !len_res-1
        | Some index -> index
      in
      dyna_res_arr.(rpz_index_in_res) <- i::dyna_res_arr.(rpz_index_in_res)
    done;
    let rec res_list i acc =
      if i >= n (* P.k *) then acc
      else match dyna_res_arr.(i) with
        | [] -> res_list (i+1) acc
        | cl -> res_list (i+1) (cl::acc)
    in
    res_list 0 []

  (* more precisely, we add the union-find produces by ill to the parameter uf *)
  let rec add_diagram_to_uf (acc: Uf.t) (d: t) : Uf.t =
    match d with
    | [] -> acc
    | (_, [])::cls | (_,[_])::cls -> (* skip *) add_diagram_to_uf acc cls
    | (_, (h::t))::_cls ->
      let rpz = Uf.find acc h (* (if rev then -h else h) *) in (* unioning with the canonical should be the fastest *)
      let rec loop_cl (acc: Uf.t) : int list -> Uf.t = function
        | [] -> acc
        | h::t -> loop_cl (Uf.union acc rpz h (* (if rev then -h else h) *)) t
      in
      let new_acc = loop_cl acc t in
      add_diagram_to_uf new_acc _cls

  let concat (a: t) (b: t) : t =
    (* check_diagram a; *)
    (* check_diagram b; *)
    (* Printf.printf "\na="; Toolbox.ll_print a; *)
    (* Printf.printf "\nb=";Toolbox.ll_print b; *)

    (* [0] initialize uf structure *)
    let uf = Uf.create (3*P.k) in

    (* [1] unify depending on a *)
    let uf = add_diagram_to_uf uf a in

    (* [2] unify depending on b *)
    let uf = add_diagram_to_uf uf (Utils.map ((+)P.k) b) in

    (* [3] extract C from uf *)
    let c = ill_of_uf uf (3*P.k) in

    (* [4] compute diagram labels*)

    (* Theses arrays stores labels of each elts of diagrams *)
    let a_labels = Array.make (2*P.k) None in
    List.iter (fun (lab, l) -> List.iter (fun x -> a_labels.(x) <- Some lab) l) a;

    let b_labels = Array.make (2*P.k) None in
    List.iter (fun (lab, l) -> List.iter (fun x -> b_labels.(x) <- Some lab) l) b;

    let get_c_cl_label (cl: int list) =
      (* cl is the final composante in c *)
      let labels = List.fold_left (fun acc n ->
          match n with
          | n when n < P.k -> Option.get a_labels.(n)::acc
          | n when n < 2*P.k -> acc
          | n -> Option.get b_labels.(n-P.k)::acc
        ) [] cl in
      P.law labels
    in

    let f =
      function
      | n when n <   P.k -> Some n
      | n when n < 2*P.k -> None
      | n    (*n < 3*P.k*) -> Some (n-P.k)
    in

    let toobig_res = of_unlabelled get_c_cl_label c in
    let good_size_res = Utils.filter_map f toobig_res in
    (* print_as_string good_size_res; *)
    Utils.sort good_size_res
    (* let res = Toolbox.ll_filter_map f c in *)
    (* Printf.printf "res=\n"; Toolbox.ll_print res; print_newline(); *)
    (* let res = of_unlabelled get_c_cl_label res in *)
    (* Utils.sort (Utils.map f ) *)
    (* check_diagram res; *)

  let (@) = concat

  let (===) d d' = (* chaque concat est triée avant d'être renvoyée et les generateurs sont triées, donc on suppose que les arguments sont triés *)
    (* Toolbox.ll_print d; Toolbox.ll_print d'; *)
     d = d'

  let e i =
    range_test i 1 (P.k-1);
    let unlabelled =
      let i = i-1 in
      let rec loop (acc: int list list) =
        function
        | j when j = P.k -> acc
        | j when j = i -> loop ([j; j+1]::[P.k+j; P.k+j+1]::acc) (j+1)
        | j when j = i+1 -> loop acc (j+1)
        | j -> loop ([j; P.k+j]::acc) (j+1)
      in
      loop [] 0
    in
    unlabelled |> of_unlabelled P.init_label |> Utils.sort

  let l i =
    range_test i 1 (P.k-1);
    let unlabelled =
      let i = i-1 in
      let rec loop (acc: int list list) =
        function
        | j when j = P.k -> acc
        | j when j = i -> loop ([j+P.k]::[j+1]::[j; P.k+j+1]::acc) (j+1)
        | j when j = i+1 -> loop acc (j+1)
        | j -> loop ([j; P.k+j]::acc) (j+1)
      in
      loop [] 0
    in
    unlabelled |> of_unlabelled P.init_label |> Utils.sort

  let r i =
    range_test i 1 (P.k-1);
    let unlabelled =
      let i = i-1 in
      let rec loop (acc: int list list) =
        function
        | j when j = P.k -> acc
        | j when j = i -> loop ([j+P.k+1]::[j]::[j+1; P.k+j]::acc) (j+1)
        | j when j = i+1 -> loop acc (j+1)
        | j -> loop ([j; P.k+j]::acc) (j+1)
      in
      loop [] 0
    in
    unlabelled |> of_unlabelled P.init_label |> Utils.sort
end
