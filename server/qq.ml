type priority = int

type 'a t =
  | Empty
  | Node of priority * 'a * 'a t * 'a t

let rec push q priority0 elt =
  match q with
  | Empty ->
    Node (priority0, elt, Empty, Empty)
  | Node (p, e, l, r) ->
    if priority0 <= p then Node (priority0, elt, push r p e, l)
    else Node (p, e, push r priority0 elt, l)

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
