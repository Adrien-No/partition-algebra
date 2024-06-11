open Lib

let cardinal_of_semigroup k =
  let module Okada = Labelled_diagram.Okada (struct let k = k end) in
  let module D = Labelled_diagram.Make(Okada) in
  let card, ngen = Okada.generate D.concat [(D.e, k-1)(* (D.s, P.k-1); (D.p, P.k-1); (D.b, P.k-1) *)] in
  (* Printf.printf "cardinal : %i. nb generateurs : %i.\n" card ngen *)
  card
