open Lwt.Infix

module type ENCODING = sig
  type value

  val to_string : value -> string
  val of_string : string -> (value, [`Msg of string]) result
end

module type S = sig
  module C : Resp_client.S
  module E : ENCODING

  type t = C.ic * C.oc
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

module Encoding = struct
  module String = struct
    type value = string

    let to_string s = s
    let of_string s = Ok s
  end

  module Json = struct
    type value = Ezjsonm.t

    let to_string j = Ezjsonm.to_string j

    let of_string s =
      try Ok (Ezjsonm.from_string s) with exc ->
        Error (`Msg (Printexc.to_string exc))
  end
end

module Make (Client : Resp_client.S) (Encoding : ENCODING) = struct
  module C = Client
  module E = Encoding

  type t = C.ic * C.oc
  type value = E.value

  let connect = C.connect

  let push client ?(priority = 100) key value =
    C.run client
      [| `Bulk "push"
       ; `Bulk key
       ; `Bulk (E.to_string value)
       ; `Bulk (string_of_int priority) |]
    >>= function
    | `String "OK" ->
      Lwt.return (Ok ())
    | `Error e ->
      Lwt.return (Error (`Msg e))
    | _ ->
      Lwt.return (Error (`Msg "unexpected response type"))

  let pop client key =
    C.run_s client [|"pop"; key|]
    >>= function
    | `Array [|`Bulk value; `Integer i|] -> (
      match E.of_string value with
      | Ok value ->
        Lwt.return (Ok (Some (value, Int64.to_int i)))
      | Error (`Msg msg) ->
        Lwt.return (Error (`Msg msg)) )
    | `Nil ->
      Lwt.return (Ok None)
    | `Error e ->
      Lwt.return (Error (`Msg e))
    | _ ->
      Lwt.return (Error (`Msg "unexpected response type"))

  let length client key =
    C.run_s client [|"length"; key|]
    >>= function
    | `Integer i ->
      Lwt.return @@ Ok i
    | `Error e ->
      Lwt.return (Error (`Msg e))
    | _ ->
      Lwt.return (Error (`Msg "unexpected response type"))

  let list client =
    C.run_s client [|"list"|]
    >>= function
    | `Array a ->
      Lwt.return @@ Ok (Array.map Resp.to_string_exn a)
    | `Error e ->
      Lwt.return (Error (`Msg e))
    | _ ->
      Lwt.return (Error (`Msg "unexpected response type"))

  let del client key =
    C.run_s client [|"del"; key|]
    >>= function
    | `String "OK" ->
      Lwt.return (Ok ())
    | `Error e ->
      Lwt.return (Error (`Msg e))
    | _ ->
      Lwt.return (Error (`Msg "unexpected response type"))
end
