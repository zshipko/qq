module type ENCODING = sig
  type value

  val to_string : value -> string
  val of_string : string -> (value, [`Msg of string]) result
end

module type S = sig
  module C : Resp_client.S
  module E : ENCODING

  type t = C.t
  type value = E.value

  val connect : C.params -> t Lwt.t

  val push :
       t
    -> ?priority:int
    -> string
    -> value
    -> (unit, [`Msg of string]) result Lwt.t

  val pop :
    t -> string -> ((value * int) option, [`Msg of string]) result Lwt.t

  val length : t -> string -> (int64, [`Msg of string]) result Lwt.t
  val list : t -> (string array, [`Msg of string]) result Lwt.t
  val del : t -> string -> (unit, [`Msg of string]) result Lwt.t
end

module Encoding : sig
  module String : ENCODING with type value = string
  module Json : ENCODING with type value = Ezjsonm.t
end

module Make (C : Resp_client.S) (E : ENCODING) :
  S with module C = C and module E = E
