

(** A Trie is represented as a list of trees *)
type 'a t 

val empty_trie : unit -> 'a t

val add_vl : 'a t -> 'a list -> 'a t

val mem_vl : 'a t -> 'a list -> bool

val rm_vl : 'a t -> 'a list -> 'a t

(** Given a list of elements (the pool) and a trie 
  * Returns a list of every possible combination from the trie *)
val get_combs : 'a list -> 'a t -> ('a list) list 
