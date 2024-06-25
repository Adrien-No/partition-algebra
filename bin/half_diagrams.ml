(* We wants to count the sum of square root of sets thats partitions where cl are made of diagrams of same traversing edges labels  *)
open Lib.Toolbox

let make (gens: Lib.Diagram.generators list) (k : int) =
  let module D = Lib.Labelled.Make(struct let k = k end) in
  let elts = D.generate gens in

  (* [0] create the partition *)
  let partition : (int list, 'a list) Hashtbl.t = Hashtbl.create k in
  let traversing_edges (d: D.t) : int list =
    let open Lib.Labelled in
    (** [is_traversing l] checks if in the cl there is at least one node from each side of the diagram *)
    let is_traversing (l: int list) : bool = List.exists ((>)k) l && List.exists ((<=)k) l in

    List.filter_map (function Unique _ -> None
                            | Few(lab, l) -> if is_traversing l then Some lab else None) d
    |> List.sort compare
  in
  List.iter (fun d ->
      let t_edges = traversing_edges d in
      (* Lib.Toolbox.string_of_int_list t_edges |> Printf.printf "traversent : [%s]\n%!"; *)
      match Hashtbl.find_opt partition t_edges with
      | None -> Hashtbl.add partition t_edges [d]
      | Some ds -> Hashtbl.replace partition t_edges (d::ds)
    ) elts;

  assert (Hashtbl.fold (fun _ l init -> List.length l + init) partition 0 = List.length elts);

  (* Hashtbl.to_seq partition |> Seq.map snd |> List.of_seq |> List.concat *)
  Hashtbl.to_seq partition
  |> Seq.map fst
  (* |> Seq.map List.to_seq *)
  (* |> Seq.concat *)
  |> List.of_seq
  |> List.sort compare
(* Hashtbl.iter (fun labels ds -> List.iter D.print ds; Lib.Toolbox.string_of_int_list labels |> Printf.printf "[%s] %!") partition *)
(* Hashtbl.length partition |> Printf.printf "\nnb cl : %i\n\n" *)

(* [1] maths *)
(* let int_sqrt () *)
(* Hashtbl.fold () *)

let print gens k =
  List.init k (fun k ->
      make gens k |> List.map (List.map string_of_int >>> String.concat "," >>> Printf.sprintf "{%s}") |> String.concat " ")
  |> String.concat "\n"
  |> Printf.sprintf "%s\n"
  |> print_string

let card gens k =
  List.init k (fun k -> make gens k |> List.length |> string_of_int)
  |> String.concat ","
  |> Printf.sprintf "Suite des cardinaux : {%s}\n"
  |> print_string

let sum_sqrt gens k =
  List.init k (fun k -> make gens k |> List.length |> int_sqrt)
  |> List.fold_left (+) 0
  |> Printf.printf "sum sqrt: %i\n"
