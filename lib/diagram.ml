(* Contains global definitions for labelled and unlabelled versions of diagrams *)

type generators =
  S | P | B | E | L | R | Id

type diagram_t

module type t = sig
  type label
  type t = diagram_t
  val of_ill : int list list -> t

  val print : t -> unit
  val print_as_string : t -> unit
  val print_empty : unit -> unit
  (* val of_ill : int list list -> t *)

  val id : t
  val s : int -> t
  val p : int -> t
  val b : int -> t
  val e : int -> t
  val l : int -> t
  val r : int -> t
  val get_generator : generators -> ((int -> t) * int) (* converts from type generators (defined in top of this file) to the associated generator in current algebra *)

  val concat : t -> t -> t
  val (@) : t -> t -> t
  val (===) : t -> t -> bool

  val generate : generators list -> t list
end
