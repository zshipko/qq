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

module Make (Client : Resp_client.S) = struct
  module C = Client

  type t = C.ic * C.oc

  open C.IO

  let connect = C.connect

  let push client ?(priority = 100) key value =
    C.run client
      [| `Bulk (`String "push")
       ; `Bulk (`String key)
       ; `Bulk (`Value value)
       ; `Bulk (`String (string_of_int priority)) |]
    >>= C.decode_s client
    >>= function
    | `String "OK" ->
      return (Ok ())
    | `Error e ->
      return (Error (`Msg e))
    | _ ->
      return (Error (`Msg "unexpected response type"))

  let pop client key =
    C.run_s client [|"pop"; key|]
    >>= C.decode client
    >>= function
    | `Array [|`Bulk (`Value value); `Integer i|] ->
      return (Ok (Some (value, Int64.to_int i)))
    | `Nil ->
      return (Ok None)
    | `Error e ->
      return (Error (`Msg e))
    | _ ->
      return (Error (`Msg "unexpected response type"))

  let length client key =
    C.run_s client [|"length"; key|]
    >>= C.decode_s client
    >>= function
    | `Integer i ->
      return @@ Ok i
    | `Error e ->
      return (Error (`Msg e))
    | _ ->
      return (Error (`Msg "unexpected response type"))

  let list client =
    C.run_s client [|"list"|]
    >>= C.decode_s client
    >>= function
    | `Array a ->
      return @@ Ok (Array.map Resp.to_string_exn a)
    | `Error e ->
      return (Error (`Msg e))
    | _ ->
      return (Error (`Msg "unexpected response type"))

  let del client key =
    C.run_s client [|"del"; key|]
    >>= C.decode_s client
    >>= function
    | `String "OK" ->
      return (Ok ())
    | `Error e ->
      return (Error (`Msg e))
    | _ ->
      return (Error (`Msg "unexpected response type"))

  let dump client =
    C.run_s client [|"dump"|]
    >>= C.decode_s client
    >>= function
    | `Bulk (`String s) ->
      return (Ok s)
    | `Error e ->
      return (Error (`Msg e))
    | _ ->
      return (Error (`Msg "unexpected response type"))

  let load client s =
    C.run_s client [|"load"; s|]
    >>= function
    | `String "OK" ->
      return (Ok ())
    | `Error e ->
      return (Error (`Msg e))
    | _ ->
      return (Error (`Msg "unexpected response type"))
end
