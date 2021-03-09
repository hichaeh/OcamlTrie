

(* A Trie is represented as a list of trees *)
type 'a trie

val empty_trie : unit -> 'a trie

val add_vlist : 'a trie -> 'a list -> 'a trie


(* val rem_vl : 'a trie -> 'a list -> 'a trie *)

(* Takes a list of strings, 
* Returns a trie with the strings inserted in it *)
val strl_to_trie : ?dtf:char trie -> string list -> char trie

val insert_str : string -> char trie -> char trie

val mem_str : string -> char trie -> bool

val mem_l : 'a list -> 'a trie -> bool

(* Given a list of elements (the pool) and a trie 
* Returns a list of every possible combination from the trie *)
val get_combs : 'a list -> 'a trie -> ('a list) list 


