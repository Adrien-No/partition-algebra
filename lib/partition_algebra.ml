(*https://ocaml.org/p/unionFind/latest/doc/UnionFind/Make/index.html*)
(* on compte à partir de 1 *)
(* /!\ vérifier le coût des opérations *)

(* optimisation : faire en O(nombre_de_classe_deq._de_c), pas O( |cl_a|+|cl_b| ) *)

module type PARTITION_ALGEBRA = sig
  type t

  val id : t

  val s_i : int -> t
  val p_i : int -> t
  val b_i : int -> t

  val concat : t -> t -> t

  val print : t -> unit
end

module PartitionAlgebra (P: sig val k : int end) : PARTITION_ALGEBRA = struct

  (* because transition table is unoriented, we wants to be sure to know *)
  type signed_int = Int of int | Int_bar of int
  (* type pos_int = Int of int *)
  (* type bar_int = Bar of int *)

  (* we need int for transition to get b' from a' (and use htbl once with have the index) *)
  (* the snd arg will be updated manually in concat, others should not be directly changed ('a elem can be unioned ) *)
  type t =  (int, int UnionFind.elem) Hashtbl.t *
            (signed_int,  signed_int) Hashtbl.t *
            (int, int UnionFind.elem) Hashtbl.t

  let int_of_signed_int (si: signed_int) =
    match si with
    | Int i -> i
    | Int_bar _ -> failwith "signed_int_conversion_error: can't get int from an int_bar"

  let int_bar_of_signed_int (si : signed_int ) =
    match si with
    | Int_bar i -> i
    | Int _ -> failwith "signed_int_conversion_error: can't get int_bar from an int"

  let list_init() = List.init P.k Int.succ

  let elem_init (positiv:bool) =
    let open UnionFind in
    List.map (fun x ->
        if positiv then
          x, make x
        else
          -x, make (-x)) (list_init())
    |> List.to_seq |> Hashtbl.of_seq

  let id : t = let h = elem_init true in (* ignore (UnionFind.union (Hashtbl.find h 2) (Hashtbl.find h 3)); *) h,
               List.combine (list_init()) (list_init()) |> List.map (fun (x, y) -> Int x, Int_bar y) |> List.to_seq |> Hashtbl.of_seq, elem_init false

  let range_test i i_min i_max =
    if i < i_min || i > i_max then failwith (Printf.sprintf "[range_test_error] %i not in [%i..%i]" i i_min i_max)

  let s_i i : t =
    range_test i 1 (P.k-1);
    elem_init true,
    List.init P.k (
      function
      | j when j = (i-1) -> (Int i, Int_bar (i+1))
      | j when j = i     -> (Int (i+1), Int_bar i)
      | n -> (Int (n+1), Int_bar (n+1))
    ) |> List.to_seq |> Hashtbl.of_seq,
    elem_init false

  let p_i i : t =
    range_test i 1 P.k;
    elem_init true,
    List.init P.k (
      function
      | j when j = i-1 -> None
      | n -> Some (Int (n+1), Int_bar (n+1))
    ) |> List.filter Option.is_some |> List.map Option.get |> List.to_seq |> Hashtbl.of_seq,
    elem_init false

  let b_i i : t =
    range_test i 1 (P.k-1);
    let h_a, h_a' = elem_init true, elem_init false in
    ignore (UnionFind.union (Hashtbl.find h_a  i) (Hashtbl.find h_a  (i+1)));
    ignore (UnionFind.union (Hashtbl.find h_a' (-i)) (Hashtbl.find h_a' (- (i+1)))); (* NOTE attention, -1 ("le suivant dans les négatifs") *)
    h_a,
    List.init P.k (
      function
      | j when j = (i-1) -> (Int i, Int_bar i)
      | j when j = i     -> (Int (i+1), Int_bar (i+1))
      | n -> (Int (n+1), Int_bar (n+1))
    ) |> List.to_seq |> Hashtbl.of_seq,
    h_a'

  (* let uf_map (a : 'a elem) (f : 'a elem -> 'a elem) = *)
  (*   let rec aux k acc = *)

  let add_to_trans trans (x, y) =
    (* we use this function to avoid forgetting adding (y, x) by doing it manually *)
    match x, y with
    | Int i, Int_bar i' ->
      Hashtbl.add trans (Int i) (Int_bar i');
      Hashtbl.add trans (Int_bar i') (Int i)
    | _ -> failwith "add_to_trans_error: wrong signed int type"
  (* | Int_bar i, Int i' -> *)
  (*   Hashtbl.add trans (Int i) (Int_bar i'); *)
  (*   Hashtbl.add trans (Int_bar i') (Int i) *)

  let find_trans trans x : signed_int option =
    Hashtbl.find_opt trans x

  let concat
      (h_a, a_trans, _h_a' : t)
      (h_b, b_trans, h_b' : t) : t =
    (* on suppose que a et b sont de bonne taille *)

    (* [0] : union les a' / b' *)

    (* [1] :  *)
    let c_trans = Hashtbl.create 1 in (* we also could use two hashtbl and remove signed_int type *) (* TODO réutiliser une ancienne table ? *)
    Hashtbl.iter (fun ant im  ->
        match ant with
          Int_bar _ -> ()
        | Int i_ant ->  (* only search cases where src of edge is in a (not a') *)
          let cl_a = Hashtbl.find h_a i_ant in
          begin
            match find_trans c_trans ant with
            | None ->
              begin
                (* eventualy adding a transition *)
                match find_trans b_trans im with
                | None ->
                  begin
                    match Hashtbl.find_opt h_b i_ant with
                    | None -> ()
                    | Some cl_b -> ignore (UnionFind.union cl_a cl_b)
                  end
                | Some Int_bar y' ->
                  begin
                    let _cl_b' = Hashtbl.find h_b' y' in
                    match find_trans c_trans (Int_bar y') with
                    | None ->
                      Hashtbl.add c_trans ant (Int_bar y')
                    (* | Some Int i *) | Some Int i_y ->
                      (* on va remonter jusqu'à l'autre x (de A) qui donne une image dans la même cl de B *)
                      (* let previous_y = Hashtbl.find b_trans (Int_bar y') in *)
                      let previous_x' = Hashtbl.find c_trans (Int i_y) in
                      let previous_x = Hashtbl.find a_trans previous_x' in
                      ignore (UnionFind.union (Hashtbl.find h_a (int_of_signed_int previous_x)) (Hashtbl.find h_a (int_of_signed_int ant)))
                    | Some Int_bar _ -> ()
                  end
                | Some Int _y' -> ()

              end
            | Some _x' -> failwith "todo / should not be rushable"
          end
      ) a_trans;
    h_a, c_trans, h_b'

    let convert si =
      match si with
      | Int i -> i
      | Int_bar i -> -i

  let to_graph (diagram: t) =
    let open Draw in
    let convert si =
      match si with
      | Int i -> i
      | Int_bar i -> -i
    in
    let h_a, edges, h_a' = diagram in
      List.fold_left (G.add_vertex) G.empty (list_init() @ (list_init() |> List.map Int.neg))
      |> Hashtbl.fold (fun key value g ->
          if UnionFind.get value <> key then
            G.add_edge g key (UnionFind.find value |> UnionFind.get)
          else g
        ) h_a
      |> Hashtbl.fold (fun key value g ->
          if UnionFind.get value <> key then
            G.add_edge g key (UnionFind.find value |> UnionFind.get)
          else g
        ) h_a'
      |> Hashtbl.fold (fun key value g ->
          G.add_edge g (convert key) (convert value)
        ) edges
      (* |> fun g -> G.add_edge g 2 (-3) *)

  let print (diagram: t) =
    let g = to_graph diagram in
    (* let g = Draw.G.add_edge g 2 (-3) in *)
    let file = open_out "/home/adriroot/Nextcloud/cours/mag/ter/factorisation-semigroupes/partition_algebra/img/diagram_test.dot" in
    (* failwith "todo "  *)Draw.Dot.output_graph file g

end
