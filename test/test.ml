module CharTrie = Trie.Make(Char)


let _ = 
 (* *)
  let dtf = CharTrie.add (CharTrie.add CharTrie.empty ['b'; 'c']) ['a'; 'b'] in 
  Printf.printf "%b\n" (CharTrie.mem dtf ['a'; 'b'; 'c']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['a']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['a'; 'b']);
  Printf.printf "%b\n" (CharTrie.mem dtf ['b'; 'c']);

  let x = CharTrie.add CharTrie.empty ['a'; 'b'; 'c'] in 
  let y = CharTrie.add x ['a';'b'] in 
  let z = CharTrie.add (CharTrie.add x ['a';'b';'c';'d']) ['b';'a'] in

  assert(CharTrie.mem x ['a'; 'b'; 'c']);
  
  assert(CharTrie.mem y ['a'; 'b'; 'c']);
  assert(CharTrie.mem y ['a'; 'b']);
  
  assert(CharTrie.mem z ['a'; 'b'; 'c'; 'd']);
  assert(CharTrie.mem z ['a'; 'b'; 'c']);
  assert(not @@ CharTrie.mem z ['a'; 'b']);
  
  assert(CharTrie.is_subtrie x z);
  assert(CharTrie.is_subtrie (CharTrie.add x ['b';'a']) z);
  assert(not @@ CharTrie.is_subtrie y z);

  Printf.printf "Tests : ok\n"
