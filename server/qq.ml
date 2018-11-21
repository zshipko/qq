type priority = int

let default_priority = 100

type 'a t =
  | Empty
  | Node of priority * 'a * 'a t * 'a t

let rec push q ?(priority = default_priority) elt =
  match q with
  | Empty ->
    Node (priority, elt, Empty, Empty)
  | Node (p, e, l, r) ->
    if priority < p then Node (priority, elt, push r ~priority:p e, l)
    else Node (p, e, push r ~priority elt, l)

let rec remove_top = function
  | Empty ->
    Empty
  | Node (_, _, l, r) -> (
    match l with
    | Empty -> (
      match r with
      | Empty ->
        l
      | Node (_, _, _, _) ->
        r )
    | Node (lp, le, _, _) -> (
      match r with
      | Empty ->
        l
      | Node (rp, re, _, _) ->
        if lp <= rp then Node (lp, le, remove_top l, r)
        else Node (rp, re, l, remove_top r) ) )

let pop q =
  match q with
  | Empty ->
    None
  | Node (p, e, _, _) ->
    Some ((p, e), remove_top q)

let empty = Empty

let is_empty = function
  | Empty ->
    true
  | Node (_, _, _, _) ->
    false

let rec length = function
  | Empty ->
    Int64.zero
  | Node (_, _, l, r) ->
    Int64.(succ (add (length l) (length r)))

module Make (Contents : Irmin.Type.S) = struct
  type 'a q = 'a t
  type t = Contents.t q

  let t =
    Irmin.Type.(
      mu (fun t ->
          variant "qq" (fun empty node -> function
            | Empty ->
              empty
            | Node (p, e, l, r) ->
              node ((p, e), l, r) )
          |~ case0 "Empty" Empty
          |~ case1 "Node"
               (triple (pair int Contents.t) t t)
               (fun ((p, e), l, r) -> Node (p, e, l, r))
          |> sealv ))

  let merge = Irmin.Merge.(option (default t))
end
