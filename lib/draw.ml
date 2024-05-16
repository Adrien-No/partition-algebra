(* from https://stackoverflow.com/questions/8999557/how-to-visualize-draw-automata-in-ocaml *)

let w_vertex, h_vertex = 1, 1.

(* let print_ill t = *)
(*   let open List in *)
(*   Printf.printf "[|"; *)
(*   iteri (fun i t -> *)
(*       iteri (fun j x -> *)
(*           Printf.printf "%i" x; *)
(*           if j <> length t -1 then *)
(*             Printf.printf " " *)
(*         ) t; *)
(*       if i  <> length t -1 then *)
(*             Printf.printf "\n" *)
(*     ) t; *)
(*   Printf.printf "|]\n" *)

let print_ill l =
  let l = List.map (fun l -> ("\t[|" ^ (l |> List.map string_of_int |> String.concat " ") ^ "|]")) l in
  "[|" ^ (String.concat "\n" l) ^ "|]\n" |> Printf.printf "%s"

(* open Ocamlgraph *)
(* representation of a node -- must be hashable *)
(* type signed_int = Int of int | Bar of int *)
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

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Neato(struct
   include G (* use the graph module from above *)
   let edge_attributes (_a, e, _b) = [`Label e; `Color 4711]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   (* let vertex_attributes v = *)

   (*   [`Shape `Box; `Pos (Printf.sprintf "%i,%i0!" x y)] (\* attributs de position manuelle *\) *)
   let vertex_attributes v =
     let x, y = (abs v) * w_vertex |> float_of_int, if v > 0 then h_vertex else 0.
     in
     [`Shape `Circle; `Pos (x,y)]
   let vertex_name (v: int) : string = string_of_int v

   let default_vertex_attributes _ = []
  let graph_attributes _ = [(* `Pagedir `LeftToRight *)]
end)

let pin_dot (file_name: string) =
  (* OCaml lib doesn't support adding parameter `!` for pinning (defining permanent pos), so we add it in the .dot file *)
  let file = open_in_bin file_name in
  let content = really_input_string file (in_channel_length file) in
  let new_content = Str.global_replace (Str.regexp "\", s") ("!\", s") content in
  close_in file;
  let file = Out_channel.open_bin file_name in
  Out_channel.output_string file new_content;
  close_out file
