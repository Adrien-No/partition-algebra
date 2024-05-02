(*https://ocaml.org/p/unionFind/latest/doc/UnionFind/Make/index.html*)

module PartitionAlgebra (P: sig val k : int end) = struct
  open UnionFind

  type 'a t =  ('a elem) * ('a * 'a) list * ('a elem)

  (* let el_of_int n = *)
  (*   if n < P.k then Int (n+1) *)
  (*   else if n < 2*P.k then Int_Bar (n+1) *)
  (*   else failwith "wrong input" *)

  (* let iterator () = *)
  (*   let get n = *)
  (*     match UnionFind.find *)
  (*   let rec aux acc = *)
  (*     let open Seq in *)
  (*     function *)
  (*     | n when n <   P.k -> *)
  (*       let new_h = return (Int (n+1)) in *)
  (*       aux (cons new_h acc) (n+1) *)
  (*     | n when n < 2*P.k -> *)
  (*       let new_h = return (Int_Bar (n+1)) in *)
  (*       aux (cons new_h acc) (n+1) *)
  (*     | _               -> acc *)
  (*   in *)

  let id =
    List.init (2*P.k) el_of_int |> List.map make |> List.fold_left union (make (el_of_int 1))

  let uf_map (a : 'a elem) (f : 'a elem -> 'a elem) =
    let rec aux k acc =

  let concat (a : t) (a' : t) =
    (* on suppose que a et a' sont de bonne taille *)

    let rec aux i a a' acc =
      if i = P.k then


end
open UnionFind

let z = union (make 2) (make 5)



let () = print_endline "Hello, World!"
