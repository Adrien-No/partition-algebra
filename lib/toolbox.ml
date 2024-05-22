let convert k x = (*from [-k; k]\{0} to [0; 2k-1] *)
  if x > 0 then x - 1
  else k - x - 1

let unconvert k i = (* from [0; 2k-1] to [-k; k]\{0}*)
  if i < k then (i+1)
  else -(i-k+1)

(* ================ basic functions for 'a list list ================ *)
let ll_map f =
  List.map (List.map f)

let ll_fold f init =
  List.fold_left (List.fold_left f) init

let ll_filter f ll =
  List.map (List.filter f) ll |> List.filter ((<>)[])

let ll_filter_map f ll =
  (* un peu coûteux en accès mémoire ? => TODO faire avec des Seq*)
  ll_map f ll
  |> ll_filter ((<>)None)
  |> ll_map Option.get

let ll_sort ll =
  List.map (List.fast_sort compare) ll |> List.fast_sort compare

let ll_print l =
  let l = List.map (fun l -> ("[" ^ (l |> List.map string_of_int |> String.concat "; ") ^ "]")) l in
  "[" ^ (String.concat ";\n" l) ^ "]\n" |> Printf.printf "%s"
