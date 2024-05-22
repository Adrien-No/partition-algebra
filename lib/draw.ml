(* from https://stackoverflow.com/questions/8999557/how-to-visualize-draw-automata-in-ocaml *)

let w_vertex, h_vertex = 1, 1

module Node : (Graph.Sig.COMPARABLE with type t = int) = struct
   type t = int
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

module type P = sig val k : int end
(* module for creating dot-files *)
module Dot (P: sig val k : int end) = Graph.Graphviz.Neato(struct

   include G (* use the graph module from above *)
   let edge_attributes (_a, e, _b) = [`Label e; `Color 4711]
   let default_edge_attributes _ = []
   let get_subgraph _ = None

   let vertex_attributes v =
     let x, y =
       if v < P.k then
         v, 1
       else
         (v-P.k), 0
     in
     (* let x, y = (if v > 0 then v else P.k+v+1) * w_vertex |> float_of_int, if v > 0 then h_vertex else 0. *)
     (* in *)
     [`Shape `Circle; `Pos (float_of_int (x+w_vertex), float_of_int (y+h_vertex))]

   let vertex_name (v: int) : string = Toolbox.unconvert P.k v |> string_of_int

  let default_vertex_attributes _ = []
  let graph_attributes _ = [`Spline true; `Sep 1.]
end)

let dot_as_graph file g k =
  let module Dot = Dot(struct let k = k end : P) in
  Dot.output_graph file g

let pin_dot_and_typo (file_name: string) =
  (* OCaml lib doesn't support adding parameter `!` for pinning (defining permanent pos), so we add it in the .dot file *)
  let file = open_in_bin file_name in
  let content = really_input_string file (in_channel_length file) in
  let new_content = Str.global_replace (Str.regexp "\", s") ("!\", s") content in
  let new_content = Str.global_replace (Str.regexp "spline") ("splines") new_content in
  close_in file;
  let file = Out_channel.open_bin file_name in
  Out_channel.output_string file new_content;
  close_out file
