open Mirage_types_lwt
open Lwt.Infix
module Dict = Map.Make (String)

module Main
    (Console : CONSOLE)
    (Conduit : Conduit_mirage.S)
    (Pclock : PCLOCK)
    (KV : Mirage_kv_lwt.RO) =
struct
  type qq =
    { mutable map : string Qq.t Dict.t
    ; mutex : Lwt_mutex.t
    ; console : Console.t
    ; pclock : Pclock.t }

  module Data = struct type data = qq end
  module Server = Resp_mirage.Server.Make (Resp_server.Auth.String) (Data)

  let get_q ctx k =
    match Dict.find_opt k ctx.map with
    | Some x ->
      x
    | None ->
      Qq.empty

  let with_q ctx k f =
    Lwt_mutex.with_lock ctx.mutex (fun () ->
        let q = get_q ctx k in
        f q
        >|= function
        | Some q ->
          ctx.map <- Dict.update k (fun _ -> Some q) ctx.map
        | None ->
          ctx.map <- Dict.remove k ctx.map )

  let push ctx client _ nargs =
    if nargs < 2 then
      Server.discard_n client nargs
      >>= fun () -> Server.invalid_arguments client
    else
      Server.recv client
      >>= fun key ->
      Server.recv client
      >>= fun item ->
      ( if nargs = 2 then Lwt.return_none
      else
        Server.recv client
        >|= fun p -> Some (Resp.to_string_exn p |> int_of_string) )
      >>= fun priority ->
      Server.finish client ~nargs 3
      >>= fun () ->
      let item = Resp.to_string_exn item in
      with_q ctx (Resp.to_string_exn key) (fun q ->
          Lwt.return_some @@ Qq.push q ?priority item )
      >>= fun () -> Server.ok client

  let pop ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv client
      >>= fun key ->
      let key = Resp.to_string_exn key in
      with_q ctx key (fun q ->
          match Qq.pop q with
          | None ->
            Server.send client `Nil >>= fun () -> Lwt.return_none
          | Some ((p, e), q) ->
            Server.send client (`Array [|`Bulk e; `Integer (Int64.of_int p)|])
            >>= fun () -> Lwt.return_some q )

  let del ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv client
      >>= fun key ->
      let key = Resp.to_string_exn key in
      with_q ctx key (fun _ -> Lwt.return_none) >>= fun () -> Server.ok client

  let length ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv client
      >>= fun key ->
      let q = get_q ctx (Resp.to_string_exn key) in
      Server.send client (`Integer (Qq.length q))

  let list ctx client _ nargs =
    Server.discard_n client nargs
    >>= fun () ->
    let x = Dict.fold (fun k _ acc -> `Bulk k :: acc) ctx.map [] in
    Server.send client (`Array (Array.of_list x))

  let wrap_lock f ctx client cmd nargs =
    Lwt_mutex.with_lock ctx.mutex (fun () -> f ctx client cmd nargs)

  let wrap_error f ctx client cmd nargs =
    Lwt.catch
      (fun () -> f ctx client cmd nargs)
      (function
        | (Failure _ | Invalid_argument _) as exc ->
          raise exc
        | exc ->
          Console.log ctx.console (Printexc.to_string exc)
          >>= fun () -> failwith "unknwon error")

  let commands =
    [ ("push", push)
    ; ("pop", pop)
    ; ("del", del)
    ; ("length", wrap_lock length)
    ; ("list", wrap_lock list) ]

  let start console conduit pclock kv _nocrypto =
    let tcp = `TCP (Key_gen.port ()) in
    Conduit.with_tls conduit
    >>= fun conduit ->
    ( match Key_gen.ssl () with
    | true ->
      let module X509 = Tls_mirage.X509 (KV) (Pclock) in
      X509.certificate kv `Default
      >|= fun cert ->
      let conf : Tls.Config.server =
        Tls.Config.server ~certificates:(`Single cert) ()
      in
      `TLS (conf, tcp)
    | false ->
      Lwt.return tcp )
    >>= fun tcp ->
    let commands = List.map (fun (k, v) -> (k, wrap_error v)) commands in
    let server =
      Server.create ?auth:(Key_gen.auth ()) ~commands (conduit, tcp)
        {map = Dict.empty; mutex = Lwt_mutex.create (); console; pclock}
    in
    let msg =
      Printf.sprintf "Running qq-server on %s:%d" (Key_gen.addr ())
        (Key_gen.port ())
    in
    Console.log console msg >>= fun () -> Server.start server
end
