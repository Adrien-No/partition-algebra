module IntMap = Map.Make(Int)

type t = { parent : (int * unit IntMap.t) array; rank : int array }

let create n =
  { parent = Array.init n (fun i -> i, IntMap.empty |> IntMap.add i ());
    rank = Array.make n 0 }

let rec find uf i : (int * unit IntMap.t)=
  let pi = uf.parent.(i) in
  if fst pi = i then
    pi
  else begin
    let ci = find uf (fst pi) in
    uf.parent.(i) <- ci; (* path compression *)
    ci
  end

let union ({ parent = p; rank = r } as uf) x y =
  let cx = find uf x in
  let cy = find uf y in
  if cx <> cy then begin
    if r.(fst cx) > r.(fst cy) then
      p.(fst cy)<- cx
    else if r.(fst cx) < r.(fst cy) then
      p.(fst cx)<- cy
    else begin
      r.(fst cx)<- r.(fst cx) + 1;
      p.(fst cy)<- cx
    end
end
