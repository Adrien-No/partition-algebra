open Utils

module UF = Uf.Make(Uf.A)
module PartitionAlgebra (P: sig val k : int end) = struct
  open P

  type t = UF.t * (int * int) list * UF.t

  let create() : t = List.init k Int.succ |> List.map UF.create

  let id = create(), create()

  let concat (a, a': t) (b, b': t) =
    let rec aux n acc =
      if n = k then acc
      else
      if UF.create

end


let () = print_endline "Hello, World!"
