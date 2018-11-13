open Mirage_types_lwt
open Lwt.Infix
module Dict = Map.Make (String)

module Main (Console : CONSOLE) (Conduit : Conduit_mirage.S) = struct
  type qq =
    { mutable map : string Resp.t Qq.t Dict.t
    ; mutex : Lwt_mutex.t }

  module Data = struct type data = qq end

  module Server =
    Resp_lwt_mirage.Server.Make (Resp_server.Auth.String) (Data)
      (Resp_lwt_mirage.Bulk.String)

  let get_q ctx k =
    match Dict.find_opt k ctx.map with
    | Some x ->
      x
    | None ->
      Qq.empty

  let set_q ctx k v = ctx.map <- Dict.update k (fun _ -> Some v) ctx.map
  let del_q ctx k = ctx.map <- Dict.remove k ctx.map

  let with_q ctx k f =
    Lwt_mutex.with_lock ctx.mutex (fun () ->
        let q = get_q ctx k in
        let q = f q in
        set_q ctx k q; Lwt.return_unit )

  let push ctx client _ nargs =
    if nargs < 2 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      Server.recv client
      >>= fun item ->
      ( if nargs = 2 then Lwt.return_none
      else
        Server.recv_s client
        >|= fun p -> Some (Resp.to_string_exn p |> int_of_string) )
      >>= fun priority ->
      with_q ctx (Resp.to_string_exn key) (fun q -> Qq.push q ?priority item)
      >>= fun () -> Server.send client (`String "OK")

  let pop ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      let key = Resp.to_string_exn key in
      Lwt_mutex.with_lock ctx.mutex (fun () ->
          let q = get_q ctx key in
          match Qq.pop q with
          | None ->
            Server.send client `Nil
          | Some ((p, e), q) ->
            set_q ctx key q;
            Server.send client (`Array [|e; `Integer (Int64.of_int p)|]) )

  let del ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      del_q ctx (Resp.to_string_exn key);
      Server.ok client

  let length ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      let q = get_q ctx (Resp.to_string_exn key) in
      Server.send client (`Integer (Qq.length q))

  let list ctx client _ nargs =
    Server.discard_n client nargs
    >>= fun () ->
    let x = Dict.fold (fun k _ acc -> `Bulk (`String k) :: acc) ctx.map [] in
    Server.send client (`Array (Array.of_list x))

  let wrap f ctx client cmd nargs =
    Lwt_mutex.with_lock ctx.mutex (fun () -> f ctx client cmd nargs)

  let commands =
    [ ("push", push)
    ; ("pop", pop)
    ; ("length", length)
    ; ("list", wrap list)
    ; ("del", wrap del) ]

  let start console conduit _nocrypto =
    Conduit.with_tls conduit
    >>= fun conduit ->
    `TCP (Ipaddr.of_string_exn (Key_gen.addr ()), Key_gen.port ())
    |> Conduit_mirage.server
    >>= fun endp ->
    let server =
      Server.create ?auth:(Key_gen.auth ()) ~commands (conduit, endp)
        {map = Dict.empty; mutex = Lwt_mutex.create ()}
    in
    let msg =
      Printf.sprintf "Running qq-server on %s:%d" (Key_gen.addr ())
        (Key_gen.port ())
    in
    Console.log console msg >>= fun () -> Server.start server
end
