

(** A Trie is represented as a list of trees *)
type 'a trie

val empty_trie : unit -> 'a trie

val add_vl : 'a trie -> 'a list -> 'a trie

val mem_vl : 'a trie -> 'a list ->  bool

val rm_vl : 'a trie -> 'a list -> 'a trie

(** Given a list of elements (the pool) and a trie 
  * Returns a list of every possible combination from the trie *)
val get_combs : 'a list -> 'a trie -> ('a list) list 


