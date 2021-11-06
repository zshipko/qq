type priority = int

type 'a t = Empty | Node of priority * 'a * 'a t * 'a t

val push : 'a t -> ?priority:priority -> 'a -> 'a t

val remove_top : 'a t -> 'a t

val pop : 'a t -> ((priority * 'a) * 'a t) option

val empty : 'a t

val is_empty : 'a t -> bool

val length : 'a t -> int64

val join : 'a t -> 'a t -> 'a t

val to_list : 'a t -> (int * 'a) list

val from_list : (int * 'a) list -> 'a t
