(* [binomial n k] is k parmis n*)
let binomial n k =
  let cache = Array.make_matrix (k+1) (n+1) (-1) in
  let rec aux k n =
    let v_cache = cache.(k).(n) in
    if v_cache <> -1 then v_cache
    else
      let res =
        match k, n with
        | 0, _ -> 1
        | x, y when x = y -> 1
        | _ -> aux k (n-1) + aux (k-1) (n-1)
      in
      cache.(k).(n) <- res;
      res
  in
  aux k n

(*[eratostene m] génère les nombres premiers jusqu'à m exclu *)

(* pas opti car on utilise trop de diviseurs (pareil que faire un test de primalité sur chacun des elements de 1 à m) *)
(* let eratostene_0 m = *)
(*   (\* pour chacun élement de 0 à m on regarde s'il divise chacun des autres élements *\) *)
(*   if m < 2 then [] else begin *)
(*   let table = List.init (m-1) Int.succ in *)
(*   let diviseurs = List.init (m-2) ((+)2) in *)
(*   List.fold_left (fun table d -> *)
(*     let rec loop l acc = *)
(*       match l with *)
(*       | [] -> acc *)
(*       | n::q -> *)
(*         if n<>d && (n mod d = 0) then loop q acc *)
(*         else loop q l *)
(*     in *)
(*     loop table [] *)
(*       ) table diviseurs *)
(*   end *)
(* let eratostene m = *)
(*   if m < 2 then [] else *)
(*     let table = Array.init (m-2) ((+)2) in *)
(*     let len = Array.length table in *)
(*     let d = ref 2 in *)
(*     while !d < m do *)
(*       for i = 0 to len-1 do *)
(*         if table.(i) > 0 then *)
(*           if table.(i) <> !d && (table.(i) mod !d = 0) then *)
(*             table.(i) <- -1 *)
(*       done; *)
(*       incr d *)
(*     done; *)
(*     Array.to_list table |> List.filter ((<>)(-1)) *)

(* let eratostene_2 m = *)
(*   if m < 2 then [] else *)
(*     let table = Array.init (m-2) ((+)2) in *)
(*     let len = Array.length table in *)
(*     let d = ref 2 in *)
(*     while !d < m-2 do *)
(*       if table.(!d) <> -1 then *)
(*       for i = 0 to len-1 do *)
(*         if table.(i) > 0 then *)
(*           if table.(i) <> !d && (table.(i) mod !d = 0) then *)
(*             table.(i) <- -1 *)
(*       done; *)
(*       incr d *)
(*     done; *)
(*     Array.to_list table |> List.filter ((<>)(-1)) *)

let eratostene m =
  let t = Array.make (m+1) true in
  t.(0) <- false;
  t.(1) <- false;
  for i = 2 to m do
    if t.(i) then
      let multiples = ref (i*i) in
      while !multiples <= m do
        t.(!multiples) <- false;
        multiples := !multiples + i
      done;
  done;
  List.init (m) Fun.id |> List.filter (fun i -> t.(i))

let time f x =
  let t1 = Unix.time() in
  let res = f x in
  Printf.printf "temps d'execution: %f\n" (Unix.time()-.t1);
  res

let prime_decomp n = (* TODO idee opti : test de primalité sur n puis eratostene sur ~ sqrt(n) *)
  match n with
  | 0 -> [0] (* failwith "can't decomp 0" *)
  | 1 -> [1]
  | _ ->
    let primes = eratostene (n+1) in
    let rec decomp primes n acc =
      match primes with
      | [] -> failwith "[error] eratostene wrongly coded"
      | p::q ->
        if n > 1 then
          if n mod p = 0 then
            decomp primes (n/p) (p::acc)
          else
            decomp q n acc
        else
          acc
    in
    decomp primes n [] |> List.rev |> List.sort(* _uniq *) compare
