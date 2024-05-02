module IntMap = Set.Make(Int)

module Cl : Set.OrderedType with type t = IntMap.t * IntMap.t = struct
  type t = IntMap.t * IntMap.t
  let compare (x, x') (y, y') =
    let cmp = IntMap.compare x x' in
    if cmp <> 0 then cmp else
      IntMap.compare y y'
end

module SetMap = Set.Make(Cl)

  (* type t = SetMap.t *)

  (* let int_map_list() = List.init P.k Int.succ |> List.map IntMap.singleton *)
  (* let id : t = List.combine (int_map_list()) (int_map_list()) |> SetMap.of_list *)

  (* let concat (x: t) (y: t) : t = *)
  (*   SetMap.find 1 x *)

module PartitionAlgebra (P: sig val k : int end) = struct
  type t = (IntMap.t * IntMap.t, unit) Hashtbl.t

  let id : t =
    List.combine
      (List.init P.k (fun i -> IntMap.singleton (i+1), IntMap.singleton (i+1)))
      (List.init P.k (fun _ -> ()))
    |> List.to_seq
    |> Hashtbl.of_seq

  let concat (p: t) (p': t) : t =
    Hashtbl.iter
  (* let concat (p: SetMap.t) (p': SetMap.t) : SetMap.t = *)
  (*   let union (x0, x1: Cl.t * Cl.t) : Cl.t * Cl.t = *)


end

  (* type t = (int list * int list) list *)

  (* let int_list() = List.init P.k (fun i -> [i+1]) *)
  (* let id : t =  List.combine (int_list()) (int_list()) *)

  (* let find_opt_subset f (e: int) (s: t) : int list option = *)
  (*   (\* renvoie la classe d'équivalence de B contenant e E A *\) *)
  (*   let rec loop l = *)
  (*     match l with *)
  (*     | [] -> None *)
  (*     | h::t -> *)
  (*       match List.find_opt ((=)e) (f h) with *)
  (*       | Some _ -> Some (f h) *)
  (*       | None -> loop t *)
  (*   in *)
  (*   loop s *)

  (* let concat x y = *)
  (*   List.map (fun (cl,cl') -> *)
  (*       match List.find_opt (fun h -> ) find_opt_subset snd *)
  (*     ) x *)
