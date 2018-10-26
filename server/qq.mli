type priority = int

type 'a t =
  | Empty
  | Node of priority * 'a * 'a t * 'a t

val push : 'a t -> priority -> 'a -> 'a t
val remove_top : 'a t -> 'a t
val pop : 'a t -> ((priority * 'a) * 'a t) option
val empty : 'a t
val is_empty : 'a t -> bool
val length : 'a t -> int64
