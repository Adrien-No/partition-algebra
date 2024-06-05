(* NOTE (à améliorer)
   - lors d'une concat l'algo a tendance a prendre l'arete du bas (on pourrait prendre la plus proche / la plus à gauche)
*)

(* NOTE Warning
   - les générateurs ne contiennent pas tous exactement tous les sommets
   - la concat prend-elle en compte les sommets isolés ? (sont-ils ajoutés au résultat ?)
*)
module type t = sig
  type t

  val of_ill : int list list -> t

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

  val print_empty : unit -> unit
end

module Make (P: sig val k : int end) : t with type t = int list list = struct (* we could remove the type t restriction but useful to make tests easier *)
  (* En interne, les diagrammes sont numérotés *)
  (* de 0 à k-1 (en haut, de gauche à droite) *)
  (* puis de k à 2k-1 (en bas, de gauche à droite) *)

  (* Les générateurs sont numérotés en externe de 1 à k *)
  type t = int list list

  (** unsafe creation from \[-k;k\]\{0} to \[0; 2*k-1\]*)
  let of_ill ll = Toolbox.ll_map (Toolbox.convert P.k) ll |> Toolbox.ll_sort

  let id = List.init P.k (fun i -> [i; P.k+i]) |> Toolbox.ll_sort(* index goes from 0 to P.k-1 and P.k*2-1 to P.k *)

  let range_test (i: int) (i_min: int) (i_max: int) : unit =
    if i < i_min || i > i_max then failwith (Printf.sprintf "[range_test_error] %i not in [%i..%i]" i i_min i_max)

  let check_diagram (d: t) : unit =
    let h = Hashtbl.create (P.k*2) in
    let range_test i = range_test i 0 (P.k*2-1) in
    Toolbox.ll_iter (fun i -> range_test i; Hashtbl.add h i ()) d;
    if List.for_all Fun.id (List.init (P.k*2) (Hashtbl.mem h))
      && d = Toolbox.ll_sort d
    then ()
    else begin
        Printf.printf "[ERROR] invariants not maintained\n";
        Toolbox.ll_print d;
        failwith "[error]" end

  let s i = (* NOTE opti : faire des générateurs pour i dans la range test, puis les appeler sans les reconstruire *)
    range_test i 1 (P.k-1);
    let i = i-1 in
    List.init P.k (
      function
      | j when j = i   -> [j  ; P.k+j+1]
      | j when j = i+1 -> [j; P.k+j-1]
      | j ->              [j; P.k+j]) |> Toolbox.ll_sort

  let p i =
    range_test i 1 P.k;
    let i = i-1 in
    let rec loop (acc: int list list) =
      function
      | j when j = P.k -> acc
      | j when j = i -> loop ([j]::[(P.k+j)]::acc) (j+1)
      | j -> loop ([j; P.k+j]::acc) (j+1)
    in
    loop [] 0
    |> Toolbox.ll_sort

  let b i =
    range_test i 1 (P.k-1);
    let i = i-1 in
    let rec loop (acc: int list list) =
      function
      | j when j = P.k -> acc
      | j when j = i -> loop ([i; i+1; P.k+i; P.k+i+1]::acc) (j+1)
      | j when j = i+1 -> loop acc (j+1) (* déjà géré au cas précédent *)
      | i -> loop ([i; P.k+i]::acc) (i+1)
    in
    loop [] 0
    |> Toolbox.ll_sort

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

      let rec loop_diagram d (g: Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Draw.Node)(Draw.Edge).t)  =
        match d with
          [] -> g
        | cl::cls ->
          let rec loop_cl prev_elem label cl g =
            match prev_elem with
            | None ->
              begin
                match cl with
                | [] -> g
                | elem::cl -> loop_cl (Some elem) elem cl g
              end
            | Some prev_elem ->
            match cl with
            | [] -> g
            | [elem] ->
              let new_label = min label elem in
              G.add_edge_e g (G.E.create prev_elem (string_of_int new_label) elem) (* pas d'appel récursif car on a fini la liste *)
            | elem::elems ->
              let new_label = min label elem in
              let new_g = G.add_edge g prev_elem elem in (* d'après le match-case précédent, il y a encore au moins un élement dans elems *)
              loop_cl (Some elem) new_label elems new_g
          in
          let new_g = loop_cl None max_int cl g in (* NOTE pas très propre le "max_int" *)
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
    | []::cls | [_]::cls -> (* skip *) add_diagram_to_uf acc cls
    | (h::t)::_cls ->
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
    let uf = add_diagram_to_uf uf (Toolbox.ll_map ((+)P.k) b) in

    (* [3] extract C from uf *)
    let c = ill_of_uf uf (3*P.k) in
    let f =
      function
      | n when n <   P.k -> Some n
      | n when n < 2*P.k -> None
      | n    (*n < 3*P.k*) -> Some (n-P.k)
    in
    let res = Toolbox.ll_filter_map f c |> Toolbox.ll_sort in
    (* check_diagram res; *)
    res

  let (@) = concat

  let (===) d d' = (* chaque concat est triée avant d'être renvoyée et les generateurs sont triées, donc on suppose que les arguments sont triés *)
    (* Toolbox.ll_print d; Toolbox.ll_print d'; *)
     d = d'

  let e i = b i @ p i @ p (i+1) @ b i (* déjà triés car les concat et autres gen sont triées *)

  let l i = s i @ p i

  let r i = p i @ s i
end
