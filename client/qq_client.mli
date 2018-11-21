module type S = sig
  module C : Resp_client.S

  type t = C.ic * C.oc

  val connect : C.params -> t C.IO.t

  val push :
       t
    -> ?priority:int
    -> string
    -> C.bulk
    -> (unit, [`Msg of string]) result C.IO.t

  val pop :
    t -> string -> ((C.bulk * int) option, [`Msg of string]) result C.IO.t

  val length : t -> string -> (int64, [`Msg of string]) result C.IO.t
  val list : t -> (string array, [`Msg of string]) result C.IO.t
  val del : t -> string -> (unit, [`Msg of string]) result C.IO.t
  val dump : t -> (string, [`Msg of string]) result C.IO.t
  val load : t -> string -> (unit, [`Msg of string]) result C.IO.t
end

module Make (C : Resp_client.S) : S with module C = C
