Printexc.record_backtrace true

open Utils

(* ================================================================ *)
module type DIAGRAM = sig
  type t

  val id : t

  val s_i : int -> t
  val p_i : int -> t
  val b_i : int -> t

  val concat : t -> t -> t

  val print : t -> unit

  val print_empty : unit -> unit
end

module Diagram (P: sig val k : int end) : DIAGRAM = struct
  type t = int list list

  let id = List.init P.k (fun i -> let i = i+1 in [i; -i]) (* index goes from 1 to P.k and -1 to -P.k *)

  let range_test (i: int) (i_min: int) (i_max: int) : unit =
    if i < i_min || i > i_max then failwith (Printf.sprintf "[range_test_error] %i not in [%i..%i]" i i_min i_max)

  let s_i i = (* NOTE opti : faire des générateurs pour i dans la range test, puis les appeler sans les reconstruire *)
    range_test i 1 (P.k-1);
    List.init P.k (
        function
        | j when j = i-1   -> [i  ; -(i+1)]
        | j when j = i -> [i+1; -i  ]
        | i -> [i+1; -(i+1)])

  let p_i i =
    range_test i 1 P.k;
    List.init P.k (
      function
      | j when j = i-1 -> []
      | i -> [i+1; -(i+1)]
    ) |> List.filter ((<>)[])

  let b_i i =
    range_test i 1 (P.k-1);
    List.init P.k (
      function
      | j when j = i-1 -> [i; i+1; -i; -(i+1)]
      | j when j = i -> []
      | i -> [i+1; -(i+1)]
    )

  let convert i = (* what a bijection *)
    if i = 0 then failwith "generator_convert: invalid index"
    else if i < 0 then i + (P.k) (* + 1 *)
    else i + P.k -1

  let unconvert i = (* from [0, 2n-1] to [-n, n]\{0} *)
    if i > P.k-1 then i-P.k+1 (* on reste côté "positif" *)
    else i-P.k (* on passe côté "négatif" *)
  open Uf_persistant

  let ill_of_uf uf k =
    let canonical_index = Array.make (k*2) None in (* NOTE opti : take n = max |a| + |b| *)
    let dyna_res_arr = Array.make (k*2) [] in
    let len_res = ref 0 in
    for i = -k to k do
      if i <> 0 then (* 0 doesn't exists in our notation *)
        let rpz = Uf.find uf i in
        (* Printf.printf "reprensentant : %i\n" rpz; *)
        (* Printf.printf "index_called: %i\n" (rpz+k); *)
        let rpz_index_in_res =
          match canonical_index.(convert rpz) with
          | None ->
            (* new canonical found *)
            canonical_index.(convert rpz) <- Some (!len_res);
            incr len_res;
            !len_res-1
          | Some index -> (* convert  *)index
        in
        (* adds i to cl of rpz *)
        (* Printf.printf "rpz_index_in_res: %i\n" rpz_index_in_res; *)
        dyna_res_arr.(rpz_index_in_res) <- i::dyna_res_arr.(rpz_index_in_res)

    done;
    let rec res_list i acc =
      if i > convert P.k then acc
      else match dyna_res_arr.(i) with
        | [] -> acc
        | cl -> res_list (i+1) (cl::acc)
    in
    res_list 0 []

  let rec loop_diagram (rev: bool) (acc: Uf.t) (d: t) : Uf.t =
    match d with
    | [] -> acc
    | []::cls | [_]::cls -> (* skip *) loop_diagram rev acc cls
    | (h::t)::_cls ->
      let rpz = Uf.find acc (if rev then -h else h) in (* unioning with the canonical should be the fastest *)
      let rec loop_cl (acc: Uf.t) : int list -> Uf.t = function
        | [] -> acc
        | h::t -> loop_cl (Uf.union acc rpz (if rev then -h else h)) t
      in
      let new_acc = loop_cl acc t in
      loop_diagram rev new_acc _cls

  let concat (a: t) (b: t) : t =
    let uf = Uf.create P.k in

    (* [1] unify depending on a *)
    Draw.print_ill a;
    Draw.print_ill (ill_of_uf uf P.k);
    let uf = loop_diagram false uf a in
    Draw.print_ill (ill_of_uf uf P.k);
    (* [2] unify depending on b *)
    let uf = loop_diagram true  uf b in (* we reverse lecture of b *)
    Draw.print_ill (ill_of_uf uf P.k);
    (* [3] extract C from uf *)

    ill_of_uf uf P.k

  let to_graph (diagram: t) =
    let open Draw in
    if diagram = [] then G.empty (* hack un peu moche pour afficher au besoin un espacement entre des diagrams *)
      (* let g = G.add_vertex G.empty 1 in *)
      (* let g = G.add_vertex g P.k in *)
      (* G.add_edge g 1 P.k *)
    else
    let g = (* G.empty *) (* test avec seulement les sommets utilisés *)
      List.fold_left
        G.add_vertex
        G.empty
        (List.init (P.k*2) unconvert)(* ((List.init P.k Int.succ) @ (List.init P.k (fun i -> -(i+1)))) *)
    in
    Printf.printf "sommets du graphe: \n";
    List.fold_left (fun g cl ->
        (* NOTE `sort cl` pour avoir un seul arc de src vers dst ? *)
        List.fold_left (function (None, g) -> fun el -> Some el, g | (Some prev, g) -> fun el -> Printf.printf "%i %i\n" prev el; Some el, G.add_edge g prev el) (None, g) cl |> snd
                      ) g diagram

  let diagram_counter = ref 0 (* NOTE marche bien seulement si une seule instance de Diagram *)
                              (* NOTE non *)
  let print (diagram: t) =
    let g = to_graph diagram in
    (* Draw.G.iter_edges (Printf.printf "(%i, %i)\n") g; *)
    (* let g = Draw.G.add_edge g 2 (-3) in *)
    (* let file = open_out (Printf.sprintf "/home/adriroot/Nextcloud/cours/mag/ter/factorisation-semigroupes/partition_algebra/img/diagram%i.dot" !diagram_counter) in *)
    let file = open_out ("/home/adriroot/Nextcloud/cours/mag/ter/factorisation-semigroupes/partition_algebra/img/diagram"^string_of_int !diagram_counter ^".dot") in
    incr diagram_counter;
    (* failwith "todo "  *)Draw.Dot.output_graph file g

  let print_empty () = print []
end
(* ================================================================ *)

module type P = sig val k : int end

module Partition = Diagram (struct let k = 3 end : P)

let _ =
  let _s = Partition.id in
  let _s' = Partition.id in
  Partition.print (_s);
  Partition.print (_s');
  Partition.print_empty();
  Partition.print (Partition.concat _s _s')
