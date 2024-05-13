(* from https://stackoverflow.com/questions/8999557/how-to-visualize-draw-automata-in-ocaml *)

let w_vertex, h_vertex = 30, 15

type node = Node of int list | Node_bar of int list
(* open Ocamlgraph *)
(* representation of a node -- must be hashable *)
module Node : ( Graph.Sig.COMPARABLE with type t = node) = struct
   type t = node
   let compare = compare
   let hash = Hashtbl.hash
   let equal = (=)
end

(* representation of an edge -- must be comparable *)
module Edge = struct
   type t = string
   let compare = compare
   let equal = (=)
   let default = ""
end

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(Node)(Edge)

(* more modules available, e.g. graph traversal with depth-first-search *)
module D = Graph.Traverse.Dfs(G)

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Neato(struct
   include G (* use the graph module from above *)
   let edge_attributes (_a, e, _b) = [`Label e; `Color 4711]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   (* let vertex_attributes v = *)

   (*   [`Shape `Box; `Pos (Printf.sprintf "%i,%i0!" x y)] (\* attributs de position manuelle *\) *)
   let vertex_attributes v =
     let x, y = (* (abs v) * w_vertex |> float_of_int *)
       match v with
       | Node [] -> 0., 1.
       | Node_bar [] -> 0., 0.
       | Node (v::_) -> float_of_int v, 1.
       | Node_bar (v::_) -> float_of_int v, 0.
       (* float_of_int (abs (v), if v > 0 then 1. else 0. *)
                (* if v > 0 then 0. else h_vertex |> float_of_int *)
     in
     [`Shape `Circle; `Pos (x,y)]
   let vertex_name (v: node) : string =
     match v with
     | Node l -> l |> List.map string_of_int |> String.concat ","
     | Node_bar l -> "_" ^ (l |> List.map string_of_int |> String.concat ",")

   let default_vertex_attributes _ = []
  let graph_attributes _ = [`Pagedir `LeftToRight]
end)
