
module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type T = 
  sig
    type v 
    type t 
    val empty : unit -> t
    val add : t -> v list -> t
    val mem : t -> v list -> bool
    val rm : t -> v list -> t
    val get_combs : t -> v list -> (v list) list 
  end

module Make (V : OrderedType) : T with type v = V.t