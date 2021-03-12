module CharTrie = Trie.Make(Char)


let _ = 
  let dtf = CharTrie.empty in 
  let dtf = CharTrie.add dtf ['a'; 'b'; 'c'] in 
  Printf.printf "%b\n" (CharTrie.mem dtf ['a'; 'b'; 'c']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['a'; 'b']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['a']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['a'; 'c']);

(*
module CharTrie = Make(Char);;

let dtf = CharTrie.empty;;

let x = CharTrie.add dtf ['a'; 'b'; 'c'];;
let y = CharTrie.add (CharTrie.add dtf ['a'; 'b'; 'c']) ['a';'b'];;
let z = CharTrie.add (CharTrie.add (CharTrie.add dtf ['a'; 'b'; 'c']) ['a';'b';'c';'d']) ['b';'a'];;

CharTrie.mem x ['a'; 'b'; 'c'];;

CharTrie.mem y ['a'; 'b'; 'c'];;
CharTrie.mem y ['a'; 'b'];;

CharTrie.mem z ['a'; 'b'; 'c'; 'd'];;
CharTrie.mem z ['a'; 'b'; 'c'];;
CharTrie.mem z ['a'; 'b'];;
*)