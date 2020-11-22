
(* ECell: End of a word *)
type 'a dt_cell = Cell of 'a | ECell of 'a

(* The subtrees are put, sorted, in a list 
 * A node holds a cell and a list of subtrees
 * A leaf holds only a cell*)
type 'a t_tree =
  | Node_dt of 'a dt_cell * 'a t_tree list
  | Leaf_dt of 'a dt_cell

(* A Trie is represented as a list of trees *)
type 'a trie = Trie of 'a t_tree list

let empty_trie () = Trie []

let get_cell_value = function
  | Cell c -> c
  | ECell c -> c
  
let get_tree_head = function
  | Node_dt (c, _) -> get_cell_value c
  | Leaf_dt (c) -> get_cell_value c

let insert_str_tree str t_tree = 
  let rec insert_cell_rec n str cell = function
    | Node_dt (ECell c, l)::t when c = get_cell_value cell 
      -> insert_str_tree_rec (n+1) str (Node_dt (ECell c, l))::t
    
    | Node_dt (Cell c, l)::t when c = get_cell_value cell 
      -> insert_str_tree_rec (n+1) str (Node_dt (cell, l))::t
    
    | Node_dt (h, l)::t when get_cell_value h < get_cell_value cell 
      -> Node_dt (h, l)::insert_cell_rec n str cell t
    | Node_dt (h, l)::t when get_cell_value h > get_cell_value cell 
      -> insert_str_tree_rec (n+1) str (Leaf_dt cell)::Node_dt (h, l)::t
    
    | Leaf_dt (ECell c)::t when c = get_cell_value cell 
      -> insert_str_tree_rec (n+1) str (Leaf_dt (ECell c))::t
    | Leaf_dt (Cell c)::t when c = get_cell_value cell 
      -> insert_str_tree_rec (n+1) str (Leaf_dt cell)::t
    
      | Leaf_dt h::t when get_cell_value h < get_cell_value cell 
      -> Leaf_dt h::insert_cell_rec n str cell t
    | Leaf_dt h::t when get_cell_value h > get_cell_value cell 
      -> insert_str_tree_rec (n+1) str (Leaf_dt cell)::Leaf_dt h::t
    
    | [] -> insert_str_tree_rec (n+1) str (Leaf_dt cell)::[]
    | _ -> assert false 
  and inter n str cell = function
    | Node_dt (h, l) -> Node_dt (h, insert_cell_rec n str cell l)
    | Leaf_dt h -> Node_dt (h, insert_cell_rec n str cell [])
  and insert_str_tree_rec n str t_tree = 
    let len = String.length str in 
      match n with 
      | n when n >= len -> t_tree
      | _ ->
        let cell = (
          if n = len - 1 
            then (ECell (String.get str n))
            else (Cell (String.get str n))) 
        in
        inter n str cell t_tree
  in
  if (String.length str = 0)
    then t_tree 
    else (
      assert (String.get str 0 = get_tree_head t_tree);
      insert_str_tree_rec 1 str t_tree)

let insert_str str tforest = 
  let rec insert_rec c str treelist = 
    if (String.length str = 1) 
      then (
        match treelist with 
        | Leaf_dt cell::t when get_cell_value cell = c -> 
          Leaf_dt (ECell c)::t
        | Node_dt (cell, l)::t when get_cell_value cell = c -> 
          Node_dt (ECell c, l)::t
        | h::t when get_tree_head h < c -> h::(insert_rec c str t)
        | h::t when get_tree_head h > c -> Leaf_dt (ECell c)::h::t
        | [] -> Leaf_dt (ECell c)::[]
        | _ -> assert false 
      ) else (
        match treelist with 
        | h::t when get_tree_head h = c -> 
          insert_str_tree str h::t
        | h::t when get_tree_head h < c -> h::(insert_rec c str t)
        | h::t when get_tree_head h > c ->
          insert_str_tree str (Leaf_dt (Cell c))::h::t
        | [] -> 
          insert_str_tree str (Leaf_dt (Cell c))::[]
        | _ -> assert false 
      ) 
  in
  match tforest with
  | Trie l -> 
    if String.length str > 0
      then Trie (insert_rec (String.get str 0) str l)
      else Trie l
        
let strl_to_trie ?(dtf = (Trie [])) strl =
  List.fold_right insert_str strl dtf

let rec get_tree_from_tl c = function
  | h::_ when get_tree_head h = c -> Some h 
  | h::t when get_tree_head h < c -> get_tree_from_tl c t 
  | _ -> None 

let get_tree c = function
  | Trie l -> get_tree_from_tl c l
  
let mem_str str trie =
  let rec mem_str_rec n str tree = 
    if (String.length str - 1 = n) 
      then (
        match tree with 
        | Node_dt (ECell c, _) when String.get str n = c -> true 
        | Leaf_dt (ECell c) when String.get str n = c -> true 
        | _ -> false 
      ) else (
        match tree with 
        | Node_dt (c, l) when String.get str n = get_cell_value c 
          -> (match (get_tree_from_tl (String.get str (n+1)) l) with
              | Some x -> mem_str_rec (n+1) str x
              | None -> false) 
        | _ -> false )
  in
  if (String.length str = 0)
    then true
    else (
      match (get_tree (String.get str 0) trie) with 
      | Some x -> mem_str_rec 0 str x
      | None -> false) 

let mem_l cl trie = 
  let rec mem_l_rec cl tree =
    match cl with 
    | h::[] -> (
      match tree with 
      | Node_dt (ECell c, _) -> h = c
      | Leaf_dt (ECell c) -> h = c
      | _ -> false)
    | h1::h2::t -> (
        match tree with 
        | Node_dt (c, l) when get_cell_value c = h1 
          -> (match (get_tree_from_tl h2 l) with 
              | Some x -> mem_l_rec (h2::t) x
              | None -> false)
        | _ -> false)       
    | [] -> true
  in (
    match cl with 
    | h::_ -> 
     (match (get_tree h trie) with
      | Some x -> mem_l_rec cl x
      | None -> false)
    | [] -> true)

let rec rm_elem elem = function 
  | h::t when h = elem -> t
  | h::t -> h::(rm_elem elem t)
  | [] -> []

(* Given a list of elements (the pool) and a trie 
 * Returns a list of every possible combination from the trie *)
let get_combs pool trie =
  let rec rec_on_tree pool tree pref uniq =
    match uniq with  
    | h::t -> ( 
      match tree with
      | Node_dt (_, l) -> (
        match get_tree_from_tl h l with
        | Some (Node_dt (ECell _, cl)) -> (
          let new_pool = rm_elem h pool in
          let new_uniq = List.sort_uniq compare new_pool in 
            (List.rev (h::pref))::
            (rec_on_tree new_pool (Node_dt (ECell h, cl)) (h::pref) new_uniq)
            @ (rec_on_tree pool tree pref t))
        | Some (Node_dt (Cell _, cl)) -> ( 
          let new_pool = rm_elem h pool in
          let new_uniq = List.sort_uniq compare new_pool in 
            (rec_on_tree new_pool (Node_dt (ECell h, cl)) (h::pref) new_uniq)
            @ (rec_on_tree pool tree pref t))
        | Some (Leaf_dt (ECell _)) -> 
          (List.rev (h::pref))::(rec_on_tree pool tree pref t)
        | _ -> rec_on_tree pool tree pref t)
      | _ -> assert false 
      )
    | [] -> []
  in
  let rec rec_on_treelist pool treelist uniq =
    match uniq with  
    | h::t -> (
      match (get_tree_from_tl h treelist) with 
      | Some tree -> 
        let new_pool = rm_elem h pool in 
        let new_uniq = List.sort_uniq compare new_pool in
          (rec_on_tree new_pool tree (h::[]) new_uniq) 
          @ (rec_on_treelist pool treelist t) 
      | None -> rec_on_treelist pool treelist t)
    | [] -> []
  in
    match trie with 
    | Trie [] -> []
    | Trie l -> 
      let uniq = List.sort_uniq compare pool in
      rec_on_treelist pool l uniq;;


