
module type OrderedType =
sig
  type t
  val compare: t -> t -> int
end

module type T = sig
  type v 
  type t 
  val empty : t
  val is_empty : t -> bool
  val add : t -> v list -> t
  val rm : t -> v list -> t
  val mem : t -> v list -> bool
  val is_subtree : t -> t -> bool
  (* val update : t -> v list -> v list -> t *)
  val get_combs : t -> v list -> (v list) list 
end

module Make (V : OrderedType) = 
  (struct

    type v = V.t

    type tt =
      | Node_dt of v * tt list
      | ENode_dt of v * tt list
      | Leaf_dt of v
      (**)

    type t = tt list

    let empty : tt list = []
    let is_empty (dtf : t) : bool = 
      match dtf with 
      | [] -> true 
      | _ -> false 

    let add (dtf : t) (vlist : v list) : t =
      let rec radd (tl : t) (vlh : v) (vlt : v list) =
        match tl with
        | Node_dt (cv, stl) :: tlt -> (
            match V.compare vlh cv = 1 with 
            | true -> Node_dt (cv, stl) :: radd tlt vlh vlt
            | false -> 
              match V.compare vlh cv = 0, vlt = [] with 
              | true, true ->
                ENode_dt (vlh, stl) :: tlt
              | true, false -> 
                Node_dt (cv, radd stl (List.hd vlt) (List.tl vlt)) :: tlt
              | false, true -> 
                Leaf_dt (vlh) :: tl
              | false, false -> 
                Node_dt (vlh, radd [] (List.hd vlt) (List.tl vlt)) :: tl)
        | ENode_dt (cv, stl) :: tlt  -> (
            match V.compare vlh cv = 1 with 
            | true -> ENode_dt (cv, stl) :: radd tlt vlh vlt
            | false -> 
              match V.compare vlh cv = 0, vlt = [] with 
              | true, true -> 
                ENode_dt (vlh, stl) :: tlt
              | true, false -> 
                ENode_dt (cv, radd stl (List.hd vlt) (List.tl vlt)) :: tlt
              | false, true -> 
                Leaf_dt (vlh) :: tl
              | false, false -> 
                Node_dt (vlh, radd [] (List.hd vlt) (List.tl vlt)) :: tl )
        | Leaf_dt cv :: tlt -> (
            match V.compare vlh cv = 1 with 
            | true -> Leaf_dt cv :: radd tlt vlh vlt
            | false ->
              match V.compare vlh cv = 0, vlt = [] with 
              | true, true -> 
                Leaf_dt (vlh) :: tlt
              | true, false -> 
                ENode_dt (cv, radd [] (List.hd vlt) (List.tl vlt)) :: tlt
              | false, true -> 
                Leaf_dt (vlh) :: tl
              | false, false -> 
                Node_dt (vlh, radd [] (List.hd vlt) (List.tl vlt)) :: tl )
        | [] -> 
          match vlt = [] with
          | true -> 
            [Leaf_dt (vlh)]
          | false -> 
            [Node_dt (vlh, radd [] (List.hd vlt) (List.tl vlt))]
      in
      match vlist with 
      | vlh :: vlt -> radd dtf vlh vlt
      | _ -> dtf 

    let rm (dtf : t)  (vl : v list) : t = 
      let rec rrm (tl : t) ?(n = 0) ?(ptd = []) ?(acc = []) (vlh : v) (vlt : v list) : t * int list option = 
        match tl with 
        | Node_dt (c, stl) :: tlt -> (
            match V.compare vlh c = 0 with 
            | true -> (
                match vlt = [] with 
                | true -> assert false 
                | false ->
                  match List.length stl > 1 with 
                  | true -> (
                      match 
                        rrm stl ~ptd:(ptd @ acc @ [n]) 
                        (List.hd vlt) (List.tl vlt) 
                      with 
                      | nstl, Some nptd -> Node_dt (c, nstl) :: tlt, Some nptd
                      | nstl, None -> Node_dt (c, nstl) :: tlt, None
                    )
                  | false -> 
                    match 
                      rrm stl ~ptd ~acc:(acc @ [n]) 
                      (List.hd vlt) (List.tl vlt) 
                    with 
                    | nstl, Some nptd -> Node_dt (c, nstl) :: tlt, Some nptd
                    | [], None -> tlt, None
                    | nstl, None -> Node_dt (c, nstl) :: tlt, None
              )
            | false -> 
              match V.compare vlh c = 1 with 
              | true -> ( 
                match rrm tlt ~n:(n + 1) ~ptd ~acc vlh vlt
                with nstl, npdt -> Node_dt (c, stl) :: nstl, npdt
              )
              | false -> assert false 
          )
        | ENode_dt (c, stl) :: tlt -> (
            match V.compare vlh c = 0 with 
            | true -> ( 
                match vlt = [] with 
                | true -> ( 
                    Node_dt (c, stl) :: tlt, Some (ptd @ acc @ [n])
                  )
                | false ->
                  match List.length stl > 1 with 
                  | true -> (
                      match 
                        rrm stl ~ptd:(ptd @ acc @ [n]) 
                        (List.hd vlt) (List.tl vlt)
                      with
                      | nstl, Some nptd -> ENode_dt (c, nstl) :: tlt, Some nptd
                      | nstl, None -> ENode_dt (c, nstl) :: tlt, None
                    )
                  | false ->  
                    match 
                      rrm stl ~ptd ~acc:(acc @ [n]) 
                      (List.hd vlt) (List.tl vlt) 
                    with 
                    | nstl, Some nptd -> Node_dt (c, nstl) :: tlt, Some nptd
                    | [], None -> Leaf_dt (c) :: tlt, None
                    | nstl, None -> Node_dt (c, nstl) :: tlt, None
              )
            | false -> 
              match V.compare vlh c = 1 with 
              | true -> (
              match rrm tlt ~n:(n + 1) ~ptd ~acc vlh vlt
              with nstl, npdt -> ENode_dt (c, stl) :: nstl, npdt
              )
              | false -> assert false 
          )
        | Leaf_dt c :: tlt -> (
            match V.compare vlh c = 0 with 
            | true -> ( 
                match vlt = [] with 
                | true -> tlt, None
                | false -> assert false 
              )
            | false -> 
              match V.compare vlh c = 1 with
              | true -> (
                match rrm tlt ~n:(n + 1) ~ptd ~acc vlh vlt
                with nstl, npdt -> Leaf_dt c :: nstl, npdt
              )
              | false -> assert false 
          )
        | [] -> assert false 
      in 
      match vl with 
      | vlh::vlt -> (fst (rrm dtf vlh vlt))
      | _ -> dtf

    let mem (dtf : t) (vl : v list) : bool =
      let rec rmem (tl : t) (vlh : v) (vlt : v list) : bool =  
        match tl with 
        | Node_dt (c, stl) :: tlt -> (
            match V.compare c vlh = 0 with
            | true -> (
                match vlt with
                | h :: t -> rmem stl h t
                | [] -> false)
            | false ->
              match V.compare vlh c = 1 with
              | true -> rmem tlt vlh vlt
              | false -> false)
        | ENode_dt (c, stl) :: tlt -> (
            match V.compare c vlh = 0 with 
            | true -> (
                match vlt with
                | h :: t -> rmem stl h t
                | [] -> true)
            | false -> 
              match vlh > c with
              | true -> rmem tlt vlh vlt
              | false -> false) 
        | Leaf_dt c :: tlt -> (
            match V.compare c vlh = 0 with 
            | true -> (
                match vlt with
                | [] -> true
                | _ -> false)
            | false -> 
              match V.compare vlh c = 1 with
              | true -> rmem tlt vlh vlt
              | false -> false) 
        | [] -> false 
      in 
      match vl with
      | h :: t -> rmem dtf h t
      | _ -> true

    let rec is_subtree (f1: t) (f2: t) : bool = 
      let gvft (tree : tt) : v =
        match tree with 
        | Node_dt (c, _) -> c
        | ENode_dt (c, _) -> c
        | Leaf_dt c -> c
      in
      match f1, f2 with
      | h1 :: tl1, h2 :: tl2 -> (
        let v1, v2 = gvft h1, gvft h2 in 
        match V.compare v1 v2 = 1 with 
        | true -> is_subtree f1 tl2
        | false -> 
          match V.compare v2 v1 = 1 with
          | true -> false 
          | false -> 
            match h1, h2 with 
            | Node_dt (_, stl1), (Node_dt (_, stl2) | ENode_dt (_, stl2))
            | ENode_dt (_, stl1), ENode_dt (_, stl2) ->
              is_subtree stl1 stl2 && is_subtree tl1 tl2
            | Leaf_dt _, (Leaf_dt _ | ENode_dt _) -> true 
            | _ -> false) 
      | [], _ -> true
      | _ -> false  

    
    let get_combs (dtf : t ) (pool : v list) =
      let rec rm_elem elem = function
        | h :: t -> (
            match V.compare h elem = 0 with 
            | true -> t
            | false -> h :: rm_elem elem t
          )
        | [] -> []
      in
      let rec get_tree_from_tl (v: v) (dtl: t) : tt option = 
        match dtl with
        | h :: t -> (
            match h with Node_dt (c, _) | ENode_dt (c, _) | Leaf_dt c ->  
            match V.compare v c = 0 with 
            | true -> Some h
            | false ->
              match V.compare v c = 1 with 
              | true -> get_tree_from_tl v t
              | false -> None )
        | _ -> None 
      in
      let rec rec_on_tree pool tree pref uniq =
        match uniq with
        | h :: t -> (
            match tree with
            | Node_dt (_, l) | ENode_dt (_, l) -> (
                match get_tree_from_tl h l with
                | Some (ENode_dt (_, _) as st) ->
                  let npool = rm_elem h pool in
                  let nuniq = List.sort_uniq compare npool in
                  List.rev (h :: pref)
                  :: rec_on_tree npool st (h :: pref) nuniq
                  @ rec_on_tree pool tree pref t
                | Some (Node_dt (_, _) as st) ->
                  let npool = rm_elem h pool in
                  let nuniq = List.sort_uniq compare npool in
                  rec_on_tree npool st (h :: pref) nuniq
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
      let rec rec_on_treelist pool trl uniq =
        match uniq with
        | h :: t -> (
            match get_tree_from_tl h trl with
            | Some (Leaf_dt _) ->
              [h] :: rec_on_treelist pool trl t
            | Some (ENode_dt (_, _) as tree) ->
              let npool = rm_elem h pool in
              let nuniq = List.sort_uniq compare npool in
              ([h] :: rec_on_tree npool tree [h] nuniq)
              @ rec_on_treelist pool trl t
            | Some tree ->
              let npool = rm_elem h pool in
              let nuniq = List.sort_uniq compare npool in
              rec_on_tree npool tree [h] nuniq
              @ rec_on_treelist pool trl t
            | None ->
              rec_on_treelist pool trl t )
        | [] ->
          []
      in
      let uniq = List.sort_uniq compare pool in
      rec_on_treelist pool dtf uniq
      (**)

  end)