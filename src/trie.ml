
(* 
module type Comparable = sig
  type t
  val compare : t -> t -> int
end
*)


type 'a tt =
  | ENode_dt of 'a * 'a tt list
  | Node_dt of 'a * 'a tt list
  | Leaf_dt of 'a

type 'a t = 'a tt list

let empty_trie () = []
(**)

let rec get_tree_from_tl (v: 'a) (dtl: 'a t) : 'a tt option = 
  match dtl with
  | h :: t -> (
    match h with Node_dt (c, _) | ENode_dt (c, _) | Leaf_dt c ->  
      match v = c with 
      | true -> Some h
      | false ->
          match v > c with 
          | true -> get_tree_from_tl c t
          | false -> None )
  | _ -> None 
(**)

let get_tree c t : 'a tt option  = 
  get_tree_from_tl c t

(**)
let add_vl (dtf : 'a t) (vlist : 'a list) : 'a t =
    let rec radd (tl : 'a t) (vlh : 'a) (vlt : 'a list) =
      match tl with
      | Node_dt (cv, stl) :: tlt | ENode_dt (cv, stl) :: tlt  -> (
          match vlh > cv with 
          | true -> Node_dt (cv, stl) :: radd tlt vlh vlt
          | false -> 
              match vlh = cv, vlt = [] with 
              | true, true -> ENode_dt (vlh, stl) :: tlt
              | true, false -> Node_dt (cv, radd stl (List.hd vlt) (List.tl vlt)) :: tlt
              | false, true -> Leaf_dt (vlh) :: Node_dt (cv, stl) :: tlt
              | false, false -> Node_dt (vlh, radd [] (List.hd vlt) (List.tl vlt)) :: Node_dt (cv, stl) :: tlt )
      | Leaf_dt cv :: tlt -> (
          match vlh > cv with 
          | true -> Leaf_dt cv :: radd tlt vlh vlt
          | false ->
              match vlh = cv, vlt = [] with 
              | true, true -> [Leaf_dt (vlh)]
              | true, false -> Node_dt (cv, radd [] (List.hd vlt) (List.tl vlt)) :: tlt
              | false, true -> Leaf_dt (vlh) :: [Leaf_dt cv]
              | false, false -> Node_dt (vlh, radd [] (List.hd vlt) (List.tl vlt)):: Leaf_dt cv :: tlt )
      | [] -> (
        match vlt = [] with
        | true -> [Leaf_dt (vlh)]
        | false -> [Node_dt (vlh, radd [] (List.hd vlt) (List.tl vlt))])
    in
    match vlist with 
    | vlh :: vlt -> radd dtf vlh vlt
    | _ -> dtf 

(**)

let rec rm_elem elem = function
  | h :: t -> (
    match h = elem with 
    | true -> t
    | false -> h :: rm_elem elem t
  )
  | [] -> []
(**)

let mem_vl (trie : 'a t) (cl : 'a list) : bool =
  let rec mem_l_rec cl tree =
    match cl with
    | [h] -> (
        match tree with
        | ENode_dt (c, _) -> h = c
        | Leaf_dt c -> h = c
        | _ -> false )
    | h1 :: h2 :: t -> (
        match tree with
        | Node_dt (c, l) when c = h1 -> (
            match get_tree_from_tl h2 l with
            | Some x -> mem_l_rec (h2 :: t) x
            | None -> false )
        | _ -> false )
    | [] -> true
  in
  match cl with
  | h :: _ -> (
      match get_tree h trie with 
      | Some x -> mem_l_rec cl x 
      | None -> false )
  | [] -> true
(**)

let rm_vl (dtf : 'a t)  (vl : 'a list) : 'a t = 
  let rec rrm (tl : 'a t) ?(n = 0) ?(ptd = []) ?(acc = []) (vlh : 'a) (vlt : 'a list) : 'a t * int list option = 
    match tl with 
    | Node_dt (c, stl) :: tlt -> (
      match vlh = c with 
      | true -> (
        match vlt = [] with 
        | true -> assert false 
        | false ->
          match List.length stl > 1 with 
          | true -> (
            match rrm stl ~ptd:(ptd @ acc @ [n]) (List.hd vlt) (List.tl vlt) with 
            | nstl, Some nptd -> Node_dt (c, nstl) :: tlt, Some nptd
            | nstl, None -> Node_dt (c, nstl) :: tlt, None
          )
          | false -> 
            match rrm stl ~ptd ~acc:(acc @ [n]) (List.hd vlt) (List.tl vlt) with 
            | nstl, Some nptd -> Node_dt (c, nstl) :: tlt, Some nptd
            | nstl, None -> Node_dt (c, nstl) :: tlt, None
      )
      | false -> 
        match vlh > c with 
        | true -> rrm tlt ~n:(n + 1) ~ptd ~acc vlh vlt 
        | false -> assert false 
    )
    | ENode_dt (c, stl) :: tlt -> (
      match vlh = c with 
      | true -> ( 
        match vlt = [] with 
        | true -> ( 
          Node_dt (c, stl) :: tlt, Some (ptd @ acc @ [n])
        )
        | false ->
          match List.length stl > 1 with 
          | true -> (
            match rrm stl ~ptd:(ptd @ acc @ [n]) (List.hd vlt) (List.tl vlt) with
            | nstl, Some nptd -> ENode_dt (c, nstl) :: tlt, Some nptd
            | nstl, None -> Node_dt (c, nstl) :: tlt, None
          )
          | false ->  
            match rrm stl ~ptd ~acc:(acc @ [n]) (List.hd vlt) (List.tl vlt) with 
            | nstl, Some nptd -> Node_dt (c, nstl) :: tlt, Some nptd
            | nstl, None -> Node_dt (c, nstl) :: tlt, None
      )
      | false -> 
        match vlh > c with 
        | true -> rrm tlt ~n:(n + 1) ~ptd ~acc vlh vlt 
        | false -> assert false 
    )
    | Leaf_dt c :: tlt -> (
      match vlh = c with 
      | true -> ( 
        match vlt = [] with 
        | true -> tlt, None
        | false -> assert false 
      )
      | false -> 
        match vlh > c with 
        | true -> rrm tlt ~n:(n + 1) ~ptd ~acc vlh vlt 
        | false -> assert false 
    )
    | [] -> assert false 
  in 
    match vl with 
    | vlh::vlt -> (fst (rrm dtf vlh vlt))
    | _ -> dtf



(**)


(* Given a list of elements (the pool) and a trie 
 * Returns a list of every possible combination from the trie *)
let get_combs pool dtf =
  let rec rec_on_tree pool tree pref uniq =
    match uniq with
    | h :: t -> (
        match tree with
        | Node_dt (_, l) | ENode_dt (_, l) -> (
            match get_tree_from_tl h l with
            | Some (ENode_dt (_, _) as subtree) ->
              let new_pool = rm_elem h pool in
              let new_uniq = List.sort_uniq compare new_pool in
              List.rev (h :: pref)
              :: rec_on_tree new_pool subtree (h :: pref) new_uniq
              @ rec_on_tree pool tree pref t
            | Some (Node_dt (_, _) as subtree) ->
              let new_pool = rm_elem h pool in
              let new_uniq = List.sort_uniq compare new_pool in
              rec_on_tree new_pool subtree (h :: pref) new_uniq
              @ rec_on_tree pool tree pref t
            | Some (Leaf_dt _) ->
              List.rev (h :: pref) :: rec_on_tree pool tree pref t
            | _ ->
              rec_on_tree pool tree pref t )
        | Leaf_dt _ ->
          [List.rev pref] )
    | [] ->
      []
  in
  let rec rec_on_treelist pool treelist uniq =
    match uniq with
    | h :: t -> (
        match get_tree_from_tl h treelist with
        | Some (Leaf_dt _) ->
          [h] :: rec_on_treelist pool treelist t
        | Some (ENode_dt (_, _) as tree) ->
          let new_pool = rm_elem h pool in
          let new_uniq = List.sort_uniq compare new_pool in
          ([h] :: rec_on_tree new_pool tree [h] new_uniq)
          @ rec_on_treelist pool treelist t
        | Some tree ->
          let new_pool = rm_elem h pool in
          let new_uniq = List.sort_uniq compare new_pool in
          rec_on_tree new_pool tree [h] new_uniq
          @ rec_on_treelist pool treelist t
        | None ->
          rec_on_treelist pool treelist t )
    | [] ->
      []
  in
    let uniq = List.sort_uniq compare pool in
    rec_on_treelist pool dtf uniq
