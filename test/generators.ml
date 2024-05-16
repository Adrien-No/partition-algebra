Printexc.record_backtrace true

open Utils

(* let init_ll k f = *)
(*   List.in *)
let ll_map f =
  List.map (List.map f)

let ll_fold f init =
  List.fold_left (List.fold_left f) init

let ll_filter f ll =
  List.map (List.filter f) ll |> List.filter ((<>)[])

let ll_filter_map f ll =
  (* un peu coûteux en accès mémoire ? *)
  ll_map f ll
  |> ll_filter ((<>)None)
  |> ll_map Option.get

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

  let id = List.init P.k (fun i -> let i = i+1 in [i; P.k-i]) (* index goes from 1 to P.k and -1 to -P.k *)

  let range_test (i: int) (i_min: int) (i_max: int) : unit =
    if i < i_min || i > i_max then failwith (Printf.sprintf "[range_test_error] %i not in [%i..%i]" i i_min i_max)

  let s_i i = (* NOTE opti : faire des générateurs pour i dans la range test, puis les appeler sans les reconstruire *)
    range_test i 1 (P.k-1);
    List.init P.k (
      function
      | j when j = i-1   -> [i  ; -P.k+(i+1)]
      | j when j = i -> [i+1; -P.k+i  ]
      | i -> [i+1; -P.k+(i+1)])

  let p_i i =
    range_test i 1 P.k;
    List.init P.k (
      function
      | j when j = i-1 -> []
      | i -> [i+1; -P.k+(i+1)]
    ) |> List.filter ((<>)[])

  let b_i i =
    range_test i 1 (P.k-1);
    List.init P.k (
      function
      | j when j = i-1 -> [i; i+1; P.k-i; -P.k+(i+1)]
      | j when j = i -> []
      | i -> [i+1; -P.k+(i+1)]
    )


  let to_graph (diagram: t) =
    let open Draw in
    if diagram = [] then G.empty (* hack un peu moche pour afficher au besoin un espacement entre des diagrams *)
    (* let g = G.add_vertex G.empty 1 in *)
    (* let g = G.add_vertex g P.k in *)
    (* G.add_edge g 1 P.k *)
    else
      let g = G.empty (* test avec seulement les sommets utilisés *)
      (* List.fold_left *)
      (*   G.add_vertex *)
      (*   G.empty *)
      (*   (List.init (P.k*2) unconvert)(\* ((List.init P.k Int.succ) @ (List.init P.k (fun i -> -(i+1)))) *\) *)
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

  let convert i = (* from [-n, n]\{0} to [0, 2n-1] *)
    if i = 0 then failwith "generator_convert: invalid index"
    else if i < 0 then i + (P.k) (* + 1 *)
    else i + P.k -1

  let unconvert i = (* from [0, 2n-1] to [-n, n]\{0} *)
    if i > P.k-1 then i-P.k+1 (* on reste côté "positif" *)
    else i-P.k (* on passe côté "négatif" *)
  open Uf_persistant

  let ill_of_uf uf k =
    let canonical_index = Array.make (k) None in (* NOTE opti : take n = max |a| + |b| *)
    let dyna_res_arr = Array.make (k) [] in
    let len_res = ref 0 in
    for i = 0 to k-1 do
      let rpz = Uf.find uf i in
      (* Printf.printf "reprensentant : %i\n" rpz; *)
      (* Printf.printf "index_called: %i\n" (rpz+k); *)
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
      if i > convert P.k then acc
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
    (* let a = ll_map (function n when n <= 0 -> convert n | n -> n) a *)
    (* and b = ll_map (function n when n <= 0 -> convert n | n -> n) b in *)
    let a = ll_map convert a
    and b = ll_map convert b in
    let uf = Uf.create (3*P.k) in

    (* [1] unify depending on a *)
    Printf.printf "uf vide:\n"; Draw.print_ill (ill_of_uf uf (2*P.k));
    Printf.printf "A =\n"; Draw.print_ill a;
    let uf = add_diagram_to_uf uf a in
    Printf.printf "B =\n"; Draw.print_ill b;

    Draw.print_ill (ill_of_uf uf (2*P.k));
    (* [2] unify depending on b *)
    let uf = add_diagram_to_uf uf (* (ll_map (function n when n < P.k -> n | n -> n+P.k) b) in (\* we reverse lecture of b *\) *)
        (ll_map ((+)P.k) b)
    in

    Printf.printf "C =\n";
    let ill = (ill_of_uf uf (3*P.k)) in
    Draw.print_ill ill;
    print ill;

(* [3] extract C from uf *)

    let c = ill_of_uf uf (3*P.k) in
    let f =
      function
      | n when n <   P.k -> Some n
      | n when n < 2*P.k -> None
      | n    (*n < 3*P.k*) -> Some (n-P.k)
    in
    let res = ll_filter_map f c in
    Draw.print_ill res;
    print res;
    let res = ll_map unconvert res in
    Draw.print_ill res;
    print res;
    res
end
(* ================================================================ *)

module type P = sig val k : int end

module Partition = Diagram (struct let k = 5 end : P)

let test1() =
  let _s = Partition.s_i 1 in
  let _s' = Partition.s_i 1 in
  Partition.print (_s);
  Partition.print (_s');
  Partition.print_empty();
  Partition.print (Partition.concat _s _s')

let test2() =
  let _s = Partition.id in
  let _s' = Partition.id in
  Partition.print (_s);
  Partition.print (_s');
  Partition.print_empty();
  Partition.print (Partition.concat _s _s')

let _ =
  test2()
