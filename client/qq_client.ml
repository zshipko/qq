module type S = sig
  module C : Resp_client.S

  type t = C.ic * C.oc

  val connect : C.params -> t C.IO.t
  val push : t -> ?priority:int -> C.bulk -> bool C.IO.t
  val pop : t -> (C.bulk * int) option C.IO.t
  val length : t -> int64 C.IO.t
end

module Make (Client : Resp_client.S) = struct
  module C = Client

  type t = C.ic * C.oc

  open C.IO

  let connect = C.connect

  let push client ?(priority = 100) value =
    C.run client
      [| `Bulk (`String "push")
       ; `Bulk (`Value value)
       ; `Integer (Int64.of_int priority) |]
    >>= C.decode client
    >>= function
    | `String "OK" ->
      return true
    | _ ->
      return false

  let pop client =
    C.run_s client [|"pop"|]
    >>= C.decode client
    >>= function
    | `Array [|`Bulk (`Value value); `Integer i|] ->
      return (Some (value, Int64.to_int i))
    | _ ->
      return None

  let length client =
    C.run_s client [|"length"|]
    >>= C.decode client
    >>= function
    | `Integer i ->
      return @@ i
    | _ ->
      return 0L
end
