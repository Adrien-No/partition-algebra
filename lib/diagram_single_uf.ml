module type DIAGRAM = sig
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
  val (=) : t -> t -> bool
  val print : t -> unit

  val print_empty : unit -> unit
end

module Diagram (P: sig val k : int end) : DIAGRAM = struct
  (* En interne, les diagrammes sont numérotés *)
  (* de 0 à k-1 (en haut, de gauche à droite) *)
  (* puis de k à 2k-1 (en bas, de gauche à droite) *)

  (* Les générateurs sont numérotés en externe de 1 à k *)
  type t = int list list

  (** unsafe creation from \[-k;k\]\{0} to \[0; 2*k-1\]*)
  let of_ill = Toolbox.ll_map (Toolbox.convert P.k)

  let id = List.init P.k (fun i -> [i; P.k+i])(* index goes from 0 to P.k-1 and P.k*2-1 to P.k *)

  let range_test (i: int) (i_min: int) (i_max: int) : unit =
    if i < i_min || i > i_max then failwith (Printf.sprintf "[range_test_error] %i not in [%i..%i]" i i_min i_max)

  let s i = (* NOTE opti : faire des générateurs pour i dans la range test, puis les appeler sans les reconstruire *)
    range_test i 1 (P.k-1);
    let i = i-1 in
    List.init P.k (
      function
      | j when j = i   -> [j  ; P.k+j+1]
      | j when j = i+1 -> [j; P.k+j-1]
      | j ->              [j; P.k+j])

  let p i =
    range_test i 1 P.k;
    let i = i-1 in
    List.init P.k (
      function
      | j when j = i -> []
      | i -> [i; P.k+i]
    ) |> List.filter ((<>)[])

  let b i =
    range_test i 1 (P.k-1);
    let i = i-1 in
    List.init P.k (
      function
      | j when j = i -> [i; i+1; P.k+i; P.k+i+1]
      | j when j = i+1 -> []
      | i -> [i; P.k+i]
    ) |> List.filter ((<>)[])

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
      List.fold_left (fun g cl ->
          (* NOTE `sort cl` pour avoir un seul arc de src vers dst ? *)
          List.fold_left (function (None, g) -> fun el -> Some el, g | (Some prev, g) -> fun el -> (* Printf.printf "%i %i\n" prev el; *) Some el, G.add_edge g prev el) (None, g) cl |> snd
        ) g diagram

  let diagram_counter = ref 0
  let print (diagram: t) =
    let g = to_graph diagram in
    let file = open_out ("/home/adriroot/Nextcloud/cours/mag/ter/factorisation-semigroupes/partition_algebra/img/diagram"^string_of_int !diagram_counter ^".dot") in
    incr diagram_counter;
    Draw.dot_as_graph file g P.k

  let print_empty () = print []

  open Uf_persistant

  let ill_of_uf uf k =
    let canonical_index = Array.make (k) None in (* NOTE opti : take n = max |a| + |b| *)
    let dyna_res_arr = Array.make (k) [] in
    let len_res = ref 0 in
    for i = 0 to k-1 do
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
      if i > P.k then acc
      else match dyna_res_arr.(i) with
        | [] -> acc
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
    Toolbox.ll_filter_map f c

  let (@) = concat

  let (=) d d' = (* NOTE pour l'instant on ne trie pas lors de la concatenation (on suppose qu'on utilise l'égalité peu souvent). *)
                 (* Si triait était trop couteux et que OCaml ne fait pas d'opti on pourrait penser à mémoriser si une liste est deja triée ou non. *)
    let d = List.map (List.sort compare) d |> List.sort compare
    and d' = List.map (List.sort compare) d' |> List.sort compare in
    (* Toolbox.ll_print d; Toolbox.ll_print d'; *)
     d = d'

  let e i = b i @ p i @ p (i+1) @ b i

  let l i = s i @ p i

  let r i = p i @ s i
end
