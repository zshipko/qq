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
    >>= C.decode client
    >>= function
    | `String "OK" ->
      return true
    | _ ->
      return false

  let pop client key =
    C.run_s client [|"pop"; key|]
    >>= C.decode client
    >>= function
    | `Array [|`Bulk (`Value value); `Integer i|] ->
      return (Some (value, Int64.to_int i))
    | _ ->
      return None

  let length client key =
    C.run_s client [|"length"; key|]
    >>= C.decode client
    >>= function
    | `Integer i ->
      return @@ i
    | _ ->
      return 0L

  let list client =
    C.run_s client [|"list"|]
    >>= C.decode client
    >>= function
    | `Array a ->
      return @@ Array.map Resp.to_string_exn a
    | _ ->
      return [||]

  let del client key =
    C.run_s client [|"del"; key|]
    >>= C.decode client
    >>= function
    | `String "OK" ->
      return true
    | _ ->
      return false
end
