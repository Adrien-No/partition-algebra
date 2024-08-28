(* vérifie que la loi de composition des monoides est bien associative *)

module PartitionMonoid = Lib.Labelled.Make(struct let k = 6 end)

open PartitionMonoid
(* open Diagram *)

let n_diff_compo = 10

let ngens = 6
let generators = [|e 1; e 2; e 3; e 4; e 5; id; s 4; s 2; s 5|]

let elts = List.init 5 (fun _ ->
    let d = ref id in
    for _ = 0 to Random.int 10 do
      if Random.bool() then
        d := !d @ generators.(Random.int 6)
      else
        d := generators.(Random.int 6) @ !d
    done;
    !d
  )

let rec compose_randomly = function
  | [] -> []
  | h::h'::t ->
    if Random.int ngens = 0 then
      compose_randomly ((h@h')::t)
    else
      compose_randomly (h::h'::t)
  | h::_ -> h

let _ =
  let different_compositions = Array.init n_diff_compo (fun _ -> compose_randomly elts) in
  for i = 0 to n_diff_compo-1 do
    for j = i+1 to n_diff_compo-1 do
      assert(different_compositions.(i) = different_compositions.(j))
    done
  done
