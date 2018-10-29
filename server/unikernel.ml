open Mirage_types_lwt
open Lwt.Infix
module Dict = Map.Make (String)

module Main (Console : CONSOLE) (Conduit : Conduit_mirage.S) = struct
  module Data = struct type data = string Resp.t Qq.t Dict.t ref end

  module Server =
    Resp_lwt_mirage.Server.Make (Resp_server.Auth.String) (Data)
      (Resp_lwt_mirage.Bulk.String)

  let get_q d k =
    match Dict.find_opt k d with
    | Some x ->
      x
    | None ->
      Qq.empty

  let set_q d k v = Dict.update k (fun _ -> Some v) d
  let del_q d k = Dict.remove k d

  let with_q d k f =
    let q = get_q d k in
    let q = f q in
    set_q d k q

  let push ctx client _ nargs =
    if nargs < 2 then Server.invalid_arguments client
    else
      Server.recv client
      >>= fun key ->
      Server.recv client
      >>= fun item ->
      ( if nargs = 2 then Lwt.return_none
      else
        Server.recv client
        >|= fun p -> Some (Resp.to_integer_exn p |> Int64.to_int) )
      >>= fun priority ->
      ctx :=
        with_q !ctx (Resp.to_value_exn key) (fun q -> Qq.push q ?priority item);
      Server.send client (`String "OK")

  let pop ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv client
      >>= fun key ->
      let q = get_q !ctx (Resp.to_value_exn key) in
      match Qq.pop q with
      | None ->
        Server.send client `Nil
      | Some ((p, e), q) ->
        ctx := set_q !ctx (Resp.to_value_exn key) q;
        Server.send client (`Array [|e; `Integer (Int64.of_int p)|])

  let del ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv client
      >>= fun key ->
      ctx := del_q !ctx (Resp.to_value_exn key);
      Server.ok client

  let length ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv client
      >>= fun key ->
      let q = get_q !ctx (Resp.to_value_exn key) in
      Server.send client (`Integer (Qq.length q))

  let list ctx client _ nargs =
    Server.discard_n client nargs
    >>= fun () ->
    let x = Dict.fold (fun k _ acc -> `Bulk (`String k) :: acc) !ctx [] in
    Server.send client (`Array (Array.of_list x))

  let commands =
    [ ("push", push)
    ; ("pop", pop)
    ; ("length", length)
    ; ("list", list)
    ; ("del", del) ]

  let start console conduit _nocrypto =
    Conduit.with_tls conduit
    >>= fun conduit ->
    `TCP (Ipaddr.of_string_exn (Key_gen.addr ()), Key_gen.port ())
    |> Conduit_mirage.server
    >>= fun endp ->
    let server =
      Server.create ?auth:(Key_gen.auth ()) ~commands (conduit, endp)
        (ref Dict.empty)
    in
    let msg =
      Printf.sprintf "Running qq-server on %s:%d" (Key_gen.addr ())
        (Key_gen.port ())
    in
    Console.log console msg >>= fun () -> Server.start server
end
