module CharTrie = Trie.Make(Char)


let _ = 
  let dtf = CharTrie.empty () in 
  let dtf = CharTrie.add dtf ['a'; 'b'; 'c'] in 
  Printf.printf "%b\n" (CharTrie.mem dtf ['a'; 'b'; 'c']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['a'; 'b']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['a']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['a'; 'c']);