open Mirage_types_lwt
open Lwt.Infix

let default_priority = 100

module Main (Console : CONSOLE) (Conduit : Conduit_mirage.S) = struct
  module Data = struct type data = string Resp.t Qq.t ref end

  module Server =
    Resp_lwt_mirage.Server.Make (Resp_server.Auth.String) (Data)
      (Resp_lwt_mirage.Bulk.String)

  let push ctx client _ nargs =
    Server.recv client
    >>= fun item ->
    ( if nargs = 1 then Lwt.return default_priority
    else Server.recv client >|= fun p -> Resp.to_integer_exn p |> Int64.to_int
    )
    >>= fun priority ->
    ctx := Qq.push !ctx priority item;
    Server.send client (`String "OK")

  let pop ctx client _ nargs =
    Server.discard_n client nargs
    >>= fun () ->
    match Qq.pop !ctx with
    | None ->
      Server.send client `Nil
    | Some ((p, e), q) ->
      ctx := q;
      Server.send client (`Array [|e; `Integer (Int64.of_int p)|])

  let length ctx client _ nargs =
    Server.discard_n client nargs
    >>= fun () -> Server.send client (`Integer (Qq.length !ctx))

  let commands = [("push", push); ("pop", pop); ("length", length)]

  let start console conduit _nocrypto =
    `TCP (Ipaddr.of_string_exn "0.0.0.0", Key_gen.port ())
    |> Conduit_mirage.server
    >>= fun endp ->
    let server =
      Server.create ?auth:(Key_gen.auth ()) ~commands (conduit, endp)
        (ref Qq.Empty)
    in
    Server.start server
end
