(** [internalize k i] converts i in a diagram of size 2k from set [-k; k]\{0} to set [0; 2k-1].
    It correspond to the external (resp internal) numerotation of diagram vertices. *)
let internalize k x =
  if x > 0 then x - 1
  else k - x - 1

(** [externalize k i] converts i in a diagram of size 2k from set [0; 2k-1] to set [-k; k]\{0}.
    It correspond to the internal (resp external) numerotation of diagram vertices. *)
let externalize k i =
  if i < k then (i+1)
  else -(i-k+1)

(* aliases *)
let convert = internalize
let unconvert = externalize

let rec carthesian_product acc l l' =
  match l with
  | [] -> acc
  | x::q -> carthesian_product (List.fold_left (fun acc x' -> (x, x')::acc) acc l') q l'
let carthesian_product l l' = carthesian_product [] l l'

let string_of_int_list l =
  List.map string_of_int l |> String.concat " "
