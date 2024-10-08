(* NOTE ne pas confondre l'opération de concaténation @ avec celle pour les listes. *)

Printexc.record_backtrace true

let print_ill l =
  let l = List.map (fun l -> ("[" ^ (l |> List.map string_of_int |> String.concat "; ") ^ "]")) l in
  "[" ^ (String.concat ";\n" l) ^ "]\n" |> Printf.printf "%s"

(* NOTE generators e, l and r are "MACROS" i.e we labelize them as concat of generators s, p and b. *)
(* e.g : with the generator l, cl [2] will have label 1 bc l 1 = s 1 @ p 1*)

(* auxiliar functions on Diagrams, used by functor Make *)

open Diagram

module Utils = struct
  let map f =
    List.map (function Unique node -> Unique (f node) | Few (lab, l) -> Few (lab, List.map f l))

  (* let filter f ll = *)
  (*   List.map (function Unique node -> if f node then [Unique node] else [] *)
  (*                    | Few(lab, l) -> *)
  (*                      let l_filtered = List.filter f l in *)
  (*                      if l_filtered <> [] then [Few(lab, l_filtered)] else []) ll |> List.filter ((<>)[]) *)
  (* let filter_map f ll = *)
  (*   map f ll *)
  (*   |> filter ((<>)(-1)) *)
  (*   |> map Option.get *)

  let sort (l: diagram) =
    List.map (function Unique node -> Unique node | Few (lab, l) -> Few (lab, List.fast_sort compare l)) l |> List.fast_sort compare (* un ordre total sur les diagrammes étiquetés *)

  let normalize (l: diagram) =
    let l = List.map (function Few (lab, l) -> Few (lab, List.sort compare l) | x -> x) l in
    List.sort compare l
end

let diagram_counter = ref 0

module Make (P: sig val k : int end) = struct
  type t = diagram

  (* let law (x: cl) (y: cl) : int = *)
  (*   match x, y with *)
  (*   | Unique n, Unique n' *)
  (*   | Unique n, Few (n',_) *)
  (*   | Few (n, _), Unique n' *)
  (*   | Few (n, _), Few (n', _) -> min (abs n) (abs n') *)

  let check (d: (cl list)) = (* check if diagram d is an Okada diagram *)
    let d = Utils.map (Toolbox.externalize P.k) d in (* mieux de convertir pour utiliser directement les propriétés telles que définies *)
    let d = List.map (function Unique node -> node, [node] | Few (lab, nodes) -> lab, nodes) d in
    let nelts_condition (d: (int * int list) list) =
      let nelts = (List.fold_left (fun acc (_, l) -> acc + List.fold_left (fun acc _ -> acc+1) 0 l) 0 d) in
      if nelts = P.k*2 then true else failwith (Printf.sprintf "cardinal_condition : expected %i, got %i " (P.k*2) nelts)
    and is_non_crossing (x, y) (x', y')=
      let x , y  = min (abs x)  (abs y ), max (abs x ) (abs y)
      and x', y' = min (abs x') (abs y'), max (abs x') (abs y') in
      if x <= y && x' <= y' || x >= y && x' >= y' then true else failwith (Printf.sprintf "edges (%i, %i) and (%i, %i) are crossing" x y x' y')
    and label_condition = function (lab, [x; y]) -> 1 <= lab && lab <= (min (abs x) (abs y)) | _ -> failwith "label_conditions: couplage imparfait"
    and parity_condition = function (lab, [x; y]) -> lab mod 2 = (min (abs x) (abs y)) mod 2 | _ -> failwith "parity_conditions: couplage imparfait"
    and nested (lab, (x, y)) (lab', (x', y')) =
      let a, b = min (abs x) (abs y) , max (abs x) (abs y)
      and c, d = min (abs x')(abs y'), max (abs x')(abs y') in
      not (c < a && a < b && b < d) (* is nested ? *)
      || lab < lab' |> fun x -> if x then true else failwith (Printf.sprintf "[nesting] (%i, %i) isn't under (%i, %i)" a b c d)
    in
    nelts_condition d
    && List.for_all (function ((lab, [x; y]), (lab', [x'; y'])) -> is_non_crossing (x, y) (x', y') && nested (lab, (x, y)) (lab', (x', y')) | _ -> failwith "couplage imparfait")
      (Toolbox.carthesian_product d d) (* /!\ expensive *)
    && List.for_all (fun cl -> label_condition cl && parity_condition cl) d

  let check_test d = if check d then () else failwith "diagramme mal formé"

  let law2 x y = min (abs x) (abs y)
  (* if Random.bool() then min (abs x) (abs y) else max (abs x) (abs y) *)
  let law_mult (l : int list) : int = List.fold_left law2 P.k l

  let of_unlabelled f ill =
    (* let f' = fun l -> List.map (fun x -> x mod P.k) l |> f in *)
    List.map (function [] -> failwith "concat: empty cl"
                     | [x] -> Unique x (* TODO we don't care of previous label ? *)
                     | l -> Few (f (List.map (Toolbox.externalize P.k) l), l)) ill

  let to_graph (diagram: t) =
    let open Draw in
    let g = List.fold_left G.add_vertex G.empty (List.init (P.k*2) Fun.id) in

    let rec loop_diagram (d: t) (g: Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Draw.Node)(Draw.Edge).t)  =
      match d with
        [] -> g
      | Unique _node::cls -> loop_diagram cls g (* _node has already been added *)
      | Few (label, cl)::cls ->
        let rec loop_cl prev_elem (cl : int list) g =
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
              G.add_edge_e g (G.E.create prev_elem (string_of_int label) elem) (* pas d'appel récursif car on a fini la liste *)
            | elem::elems ->
              let new_g = G.add_edge g prev_elem elem in (* d'après le match-case précédent, il y a encore au moins un élement dans elems *)
              loop_cl (Some elem) elems new_g
        in
        let new_g = loop_cl None cl g in
        loop_diagram cls new_g
    in
    loop_diagram diagram g

  let print (diagram: t) =
    (* check_test diagram; *)
    let g = if diagram = [] then Draw.G.empty else to_graph diagram in
    let file =
      try (* depending on the caller-folder (test or bin), the path isn't the same  *)
        open_out (Sys.getcwd() ^ "/../../../../img/diagram"^string_of_int !diagram_counter ^".dot")
      with Sys_error _ ->
        open_out (Sys.getcwd() ^ "/img/diagram"^string_of_int !diagram_counter ^".dot")
    in
    incr diagram_counter;
    Draw.dot_as_graph file g P.k

  let to_string ?(intern=false) d =
    let d = if not intern then
        Utils.map (Toolbox.externalize P.k) d else d in
    let cl_to_string = function
      | Unique node -> string_of_int node
      | Few (label, nodes) -> ("(" ^ string_of_int label ^ ") [" ^ (nodes |> List.map string_of_int |> String.concat "; ") ^ "]")
    in
    "[" ^ (String.concat ";\n" (List.map cl_to_string d)) ^ "]\n"

  open Uf_persistant
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

  let rec add_diagram_to_uf (acc: Uf.t) (d: t) : Uf.t =
    match d with
    | [] -> acc
    | Unique _ ::cls | Few (_, [])::cls -> (* skip *) add_diagram_to_uf acc cls
    | Few (_, (h::t))::_cls ->
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
    let uf = add_diagram_to_uf uf (Utils.map ((+)P.k) b) in

    (* [3] extract unlabelled C from uf. *)
    (* we use an "unprocessed" state for nodes (shorted "up"), where : *)
    (* elts a_i are 0..P.k-1, elts a_i\ *)
    (*      b_i are P.k..2*P.k-1 *)
    (*     b_i\ are 2*P.k..3*P.k-1 *)
    let unlab_unprocessed_c = ill_of_uf uf (3*P.k) in

    (* [4] labelize C *)
    (* [4.1] storing the label of each node (max_int or default value if the node is lonely)*)
    let a_labels = Array.make (2*P.k) max_int in
    List.iter (function Few(lab, l) -> List.iter (fun i -> a_labels.(i) <- lab) l | _ -> ()) a;
    let b_labels = Array.make (2*P.k) max_int in
    List.iter (function Few(lab, l) -> List.iter (fun i -> b_labels.(i) <- lab) l | _ -> ()) b;

    (* [4.2] building c that is labelized *)
    let process_node = (* goes from unprocessed node to final node *)
      function
      | n when n <   P.k -> Some n
      | n when n < 2*P.k -> None
      | n    (*n < 3*P.k*) -> Some (n-P.k)
    in
    let c_labels = Array.init (2*P.k) Fun.id in
    (* let c_labels = Array.make (2*P.k) max_int in *)
    List.iter (function [up_n] -> (* Unique up_n *) ()
                      | [] -> failwith "can't be empty"
                      | l ->
                        let labels = List.map (fun node ->
                            if      node < P.k then (* a_i *)
                              a_labels.(node)
                            else if node < 2*P.k then (* a_i\ or b_i *)
                              min a_labels.(node) b_labels.(node-P.k)
                            else (* b_i\ *)
                              b_labels.(node-P.k)
                          ) l
                        in
                        let lab = List.fold_left min max_int labels in
                        List.iter (fun node ->
                            if node < P.k then
                              c_labels.(node) <- lab
                            else if node < 2*P.k then
                              ()
                            else
                              c_labels.(node-P.k) <- lab
                          ) l
      ) unlab_unprocessed_c;
    let unlab_c = Unlabelled.Utils.filter_map process_node unlab_unprocessed_c in
    let c = List.map (function
        | [n] -> Unique n
        | [] -> failwith "can't be empty"
        | l -> assert (List.for_all ((=)c_labels.(List.hd l)) (List.map (fun i -> c_labels.(i)) l));
          Few (c_labels.(List.hd l), l)
      ) unlab_c
    in Utils.sort c

  let generator_builder i imax (f: int -> int list list) =
    let range_test (i: int) (i_min: int) (i_max: int) : unit =
      if i < i_min || i > i_max then failwith (Printf.sprintf "[range_test_error] %i not in [%i..%i]" i i_min i_max)
    in
    range_test i 1 imax;
    let unlabelled =
      let rec loop (acc: int list list) j =
        if j = P.k then acc
        else loop (f j @ acc) (j+1)
      in loop [] 0
    in unlabelled |> of_unlabelled law_mult |> Utils.sort

  let id = generator_builder (P.k+1) (P.k+1) (fun i -> [[i; P.k+i]])

  let s i = generator_builder i (P.k-1) (function
      | j when j = i-1   -> [[j  ; P.k+j+1]]
      | j when j = i -> [[j; P.k+j-1]]
      | j ->              [[j; P.k+j]])

  let p i = generator_builder i P.k (function
      | j when j = i-1 -> [[j]; [P.k+j]]
      | j -> [[j; P.k+j]]) |> Utils.sort

  let b i = generator_builder i (P.k-1) (function
      | j when j = i-1 -> [[j; j+1; P.k+j; P.k+j+1]]
      | j when j = i -> []
      | i -> [[i; P.k+i]])

  let e i = generator_builder i (P.k-1) (function
      | j when j = i-1 -> [[j; j+1];[P.k+j; P.k+j+1]]
      | j when j = i -> []
      | j -> [[j; P.k+j]])

  let l i = generator_builder i (P.k-1) (function
      | j when j = i-1 -> [[j+P.k];[j+1];[j; P.k+j+1]]
      | j when j = i -> []
      | j -> [[j; P.k+j]])

  let r i = generator_builder i (P.k-1) (function
      | j when j = i-1 -> [[j+P.k+1];[j];[j+1; P.k+j]]
      | j when j = i -> []
      | j -> [[j; P.k+j]])

  let get_generator =
    let open Diagram in
    function
    | S -> s, P.k-1
    | P -> p, P.k
    | B -> b, P.k-1
    | E -> e, P.k-1
    | L -> l, P.k-1
    | R -> r, P.k-1
    | Id -> (fun _ -> id), 1

  let generate (gens': Diagram.generators list) =
    let open Diagram in
    let gens =
      let generate_generators f imax =
        if imax >= 0 then List.init imax Int.succ |> List.map f
        else []
      in
      List.concat (List.map (fun (f, imax) -> generate_generators f imax) (List.map get_generator gens'))
      |> List.map Utils.sort
    in
    (* Printf.printf "%i générateurs et id :\n" (List.length gens); *)
    (* List.iter (fun p -> to_string p |> Printf.printf "%s\n") gens; *)
    Generate_semigroup.make gens concat (* Utils.sort *) Utils.normalize to_string
  (* Generate_semigroup.make3 (List.combine gens gens') concat *)
  (* Generate_semigroup.make4 gens concat id *)

  let (===) = (=)
  let (@@@) = concat
  let (@) = concat

  let externalize node = Toolbox.externalize P.k node
  let internalize node = Toolbox.internalize P.k node

  let length (d: diagram) =
    let compute_edge = function
      | Few (lab, [src; dst]) -> (externalize src |> abs) + (externalize dst |> abs) - (2 * lab)
      | _ -> failwith "unimplemented" (* for now we only works with temperley-lieb *)
    in
    List.map compute_edge d
    |> List.fold_left (+) 0
    |> (/)2
end


let print_list k gens =
  List.init k (fun k ->
      let module M = Make (struct let k = k end) in
      M.generate gens |> List.length |> string_of_int
    ) |> String.concat "," |> Printf.printf "{%s}\n"


let to_string (d: (int * (int * int)) list) =
  List.map (fun (label, (x,y)) -> Printf.sprintf "(%i, (%i, %i))" label x y) d |> String.concat " "
  |> Printf.sprintf "[%s]\n"

let max3 x y z = max (max x y) z

(* ==== NOTE doesn't works (used older incorrect factorization alg.) ====*)
let factorize_right (d: diagram) k = (* NOTE can really be optimized in term of constant factor *)
  let d = List.map (function  Few (lab, [src; dst]) -> lab, (Toolbox.externalize k src, Toolbox.externalize k dst)  | _ -> failwith "not perfect matching diagram") d in
  let rec aux d k =
    print_string (to_string d);
    (* we suppose that we are in Temperley-Lieb, with an externalized diagram *)
    let rec larger_descent d acc : int = (* we could don't know if/how d is sorted so we explore all the diagram *)
      match d with
      | (lab, (src, dst))::q -> if src < 0 && dst < 0 && lab = abs src && abs dst = (abs src)+1 then larger_descent q (max acc lab) else larger_descent q acc
      | [] -> acc
      (* | _ -> failwith "not perfect matching diagram" *)
    in
    let ldes = larger_descent d min_int in
    Printf.printf "k= %i, ldes= %i\n" k ldes;
    let rec test_k_propagating d =
      match d with
      | (lab, (src, dst))::q when lab = k && lab = src && src = -dst -> (* Printf.printf "(lab=%i, (src=%i, dst=%i))\n" lab src dst; *) true
      | _::q -> test_k_propagating q
      | [] -> false
    in
    let restrict = List.fold_left (fun init (lab, edge) -> if lab = k && edge = (k, -k) then init else (lab, edge)::init) [] in (* TODO à débugger *)
    if k < 2 (* || ldes < 1 *) then
      (* deja factorisé *)
      []
    else if test_k_propagating d || ldes < 1 then begin Printf.printf "test propagating\n"; aux (restrict d) (k-1) end
    else
      let b_transform j =
        if j = k then -(k-1)
        else j
      and b_bar_transform j =
        if j > ldes then -(j-2) else j
      in
      let sort_edge x y =
        if abs x < abs y then x, y else y, x
        (* min x y, max x y *)
        (* if x < 0 && y > 0 then y, x *)
        (* else if x > 0 && y < 0 then x, y *)
        (* else failwith (Printf.sprintf "edge (%i, %i) can't be sorted" x y) *)
      in
      let surge_edge = function
        | (lab, (src, dst)) when lab = ldes (* && src = -ldes && dst = -ldes *) -> (k, (k, -k))
        | (lab, (src, dst)) -> (lab, (sort_edge (b_transform src) (b_bar_transform dst)))
        (* | _ -> failwith "unfound edge, not perfect matching diagram" *)
      in
      let new_d = List.map surge_edge d in
      let restricted = restrict new_d in
      aux restricted (k-1) @ List.init (k-ldes) (fun i ->
          let i = k - i -1 in (i(* , (E : Diagram.generators) *))
        )
  in
  aux d k
