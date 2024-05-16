(* copied from https://www.lri.fr/~filliatr/ftp/publis/puf-wml07.pdf *)

Printexc.record_backtrace true

module type PersistentUnionFind = sig
  type t

  val create : int -> t (** *)

  val find : t -> int -> int (** [find s x] returns the representative of the equivalence class of x *)

  val union : t -> int -> int -> t (** *) (* Question : Is it better to union with the canonical ? -> Yes probably bc it lowers tree height *)
end

module type PersistentArray = sig
  type 'a t
  val init : int -> (int -> 'a) -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> 'a t
end

(* crée une structure UF avec des élements entiers positifs *)
module Make(A : PersistentArray) : PersistentUnionFind = struct
  type t = {
    rank: int A.t;
    mutable parent: int A.t;
  }

  let create n = {
    rank = A.init n (Fun.const 0);
    parent = A.init n Fun.id
  }

  let find h x =
    let rec aux f i =
      let fi = A.get f i in
      if fi = i then
        f, i
      else
        let f, r = aux f fi in
        let f = A.set f i r in
        f, r
    in
    let f, cx = aux h.parent x in
    h.parent <- f;
    cx

  let union h x y =
    let cx = find h x in
    let cy = find h y in
    if cx != cy then begin (* TODO <> ? *)
      let rx = A.get h.rank cx in
      let ry = A.get h.rank cy in
      if rx > ry then
        { h with parent = A.set h.parent cy cx }
      else if rx < ry then
        { h with parent = A.set h.parent cx cy }
      else
        { rank = A.set h.rank cx (rx + 1);
          parent = A.set h.parent cy cx }
    end else
      h
end

module A : PersistentArray = struct
  type 'a t = 'a data ref
  and 'a data =
    | Arr of 'a array
    | Diff of int * 'a * 'a t
    | Invalid

  let init (n: int) (f: int -> 'a) : 'a t = ref (Arr (Array.init n f))

  let rec reroot t = match !t with
    | Arr _ -> ()
    | Diff (i, v, t') -> reroot t';
      begin match !t' with
        | Arr a as n ->
          a.(i) <- v;
          t := n;
          t' := Invalid
        | Diff _ | Invalid -> assert false
      end
    | Invalid -> assert false

  let (* rec  *) get (t: 'a t) (i: int) : 'a = match !t with
    | Arr a -> a.(i)
    | Diff _ ->
      reroot t;
      begin match !t with
        | Arr a -> a.(i)
        | Diff _ | Invalid -> assert false
      end
    | Invalid -> assert false

  let set (t: 'a t) (i: int) (v: 'a) : 'a t =
    reroot t;
    match !t with
    | Arr a as n ->
      let old = a.(i) in
      a.(i) <- v;
      let res = ref n in
      t := Diff (i, old, res);
      res
    | Diff _ -> assert false
    | Invalid -> assert false
end

(* crée une structure UF avec des entiers signés non nuls *)
module Make_signed(A: PersistentArray) : PersistentUnionFind = struct
  module Uf = Make(A)
  type t = Uf.t

  let size_n = ref None

  let create n =
    size_n := Some n; (* need to know n to realise transformationss *)
    Uf.create (n) (* we go from [-n; n]\{0} to [0; 2n-1] *)

  let convert i = (* what a bijection *)
    if i = 0 then failwith "uf_persistant_convert: invalid index"
    else if i < 0 then i + (Option.get !size_n) (* + 1 *)
    else i + (Option.get !size_n) -1

  let unconvert i = (* from [0, 2n-1] to [-n, n]\{0} *)
    let n = (Option.get !size_n) in
    if i > n-1 then i-n+1 (* on reste côté "positif" *)
    else i-n (* on passe côté "négatif" *)

  (* let convert = Fun.id *)
  (* let unconvert = Fun.id *)

  let find (h: t) (x: int) : int = Uf.find h (convert x) |> unconvert

  let union (h: t) (x: int) (y: int) : t = Uf.union h (convert x) (convert y)
end

module Uf = Make(A)
