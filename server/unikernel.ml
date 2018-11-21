open Mirage_types_lwt
open Lwt.Infix
module Dict = Map.Make (String)
module Contents = Qq.Make (Irmin.Contents.String)
module Store = Irmin_mirage.Git.Mem.KV (Contents)
module Sync = Irmin.Sync (Store)

module Main
    (Console : CONSOLE)
    (Conduit : Conduit_mirage.S)
    (Pclock : PCLOCK)
    (KV : Mirage_kv_lwt.RO) =
struct
  type qq =
    { repo : Store.repo
    ; mutable branch : Store.branch
    ; pclock : Pclock.t
    ; console : Console.t }

  module Data = struct type data = qq end

  module Server =
    Resp_lwt_mirage.Server.Make (Resp_server.Auth.String) (Data)
      (Resp_lwt_mirage.Bulk.String)

  let get_store ctx = Store.of_branch ctx.repo ctx.branch

  let get_q tree k =
    Store.Tree.find tree k
    >|= function
    | Some x ->
      x
    | None ->
      Qq.empty

  module Info = Irmin_mirage.Info (struct let name = "qq" end) (Pclock)

  let set_q tree k v = Store.Tree.add tree k v
  let del_q tree k = Store.Tree.remove tree k

  let with_q ctx ?(strategy = `Set) k f =
    get_store ctx
    >>= fun t ->
    let info =
      Info.f ctx.pclock "Update queue: %s" (Irmin.Type.to_string Store.key_t k)
    in
    Store.with_tree_exn t ~info ~strategy k (fun tree ->
        let tree =
          match tree with
          | Some t ->
            t
          | None ->
            Store.Tree.empty
        in
        get_q tree Store.Key.empty
        >>= fun q ->
        f q
        >>= function
        | Some q ->
          set_q tree Store.Key.empty q >>= Lwt.return_some
        | None ->
          del_q tree Store.Key.empty >>= Lwt.return_some )

  let make_key resp =
    match Irmin.Type.of_string Store.key_t @@ Resp.to_string_exn resp with
    | Ok key ->
      key
    | Error (`Msg e) ->
      failwith ("make_key:" ^ e)

  let push ctx client _ nargs =
    if nargs < 2 then
      Server.discard_n client 1 >>= fun () -> Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      Server.recv_s client
      >>= fun item ->
      let item = Resp.to_string_exn item in
      ( if nargs = 2 then Lwt.return_none
      else
        Server.recv_s client
        >|= fun p -> Some (Resp.to_string_exn p |> int_of_string) )
      >>= fun priority ->
      Server.finish client ~nargs 3
      >>= fun () ->
      with_q ctx (make_key key) (fun q ->
          Lwt.return_some @@ Qq.push q ?priority item )
      >>= fun () -> Server.send client (`String "OK")

  let pop ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      Server.finish client ~nargs 1
      >>= fun () ->
      let key = make_key key in
      with_q ctx key (fun q ->
          match Qq.pop q with
          | None ->
            Server.send client `Nil >|= fun () -> Some q
          | Some ((p, e), q) ->
            Server.send client
              (`Array [|`Bulk (`Value e); `Integer (Int64.of_int p)|])
            >|= fun () -> Some q )

  let del ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      Server.finish client ~nargs 1
      >>= fun () ->
      let key = make_key key in
      with_q ctx key (fun _ -> Lwt.return_none) >>= fun () -> Server.ok client

  let length ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      Server.finish client ~nargs 1
      >>= fun () ->
      let key = make_key key in
      with_q ctx key (fun q ->
          Server.send client (`Integer (Qq.length q)) >|= fun () -> Some q )

  let rec list_store store key acc =
    Store.list store key
    >>= Lwt_list.fold_left_s
          (fun acc -> function
            | step, `Contents ->
              let key = Store.Key.rcons key step in
              let key = Irmin.Type.to_string Store.key_t key in
              let key = String.sub key 1 (String.length key - 1) in
              Lwt.return @@ (`Bulk (`String key) :: acc)
            | step, `Node ->
              let key = Store.Key.rcons key step in
              list_store store key [] >|= fun l -> l @ acc )
          acc

  let list ctx client _ nargs =
    Server.finish client ~nargs 0
    >>= fun () ->
    get_store ctx
    >>= fun t ->
    list_store t Store.Key.empty []
    >>= fun x -> Server.send client (`Array (Array.of_list x))

  let branch ctx client _ nargs =
    if nargs = 0 then Server.send client (`Bulk (`String ctx.branch))
    else
      Server.recv_s client
      >>= fun branch ->
      let branch = Resp.to_string_exn branch in
      Store.Branch.mem ctx.repo branch
      >>= (function
            | true ->
              Lwt.return_unit
            | false -> (
              get_store ctx
              >>= fun t ->
              Store.Head.find t
              >>= function
              | Some commit ->
                Store.Branch.set ctx.repo branch commit
              | None ->
                Lwt.return_unit ))
      >>= fun () ->
      ctx.branch <- branch;
      Server.finish client ~nargs 1 >>= fun () -> Server.ok client

  let commands =
    [ ("push", push)
    ; ("pop", pop)
    ; ("length", length)
    ; ("list", list)
    ; ("del", del)
    ; ("branch", branch) ]

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
    Store.Repo.v (Irmin_mem.config ())
    >>= fun repo ->
    let server =
      Server.create ?auth:(Key_gen.auth ()) ~commands (conduit, tcp)
        {repo; branch = Store.Branch.master; pclock; console}
    in
    let msg =
      Printf.sprintf "Running qq-server on %s:%d" (Key_gen.addr ())
        (Key_gen.port ())
    in
    Console.log console msg >>= fun () -> Server.start server
end
