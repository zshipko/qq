module type S = sig
  module C : Resp_client.S

  type t = C.ic * C.oc

  val connect : C.params -> t C.IO.t
  val push : t -> ?priority:int -> string -> C.bulk -> bool C.IO.t
  val pop : t -> string -> (C.bulk * int) option C.IO.t
  val length : t -> string -> int64 C.IO.t
  val list : t -> string array C.IO.t
  val del : t -> string -> bool C.IO.t
end

module Make (C : Resp_client.S) : S with module C = C
