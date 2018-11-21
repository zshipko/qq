open Mirage_types_lwt
open Lwt.Infix
module Dict = Map.Make (String)

module Resp_contents = struct
  type t = string Resp.t

  let t =
    Irmin.Type.(
      mu (fun t ->
          variant "Resp"
            (fun nil integer string error array bulk_string bulk_value (x : string Resp.t) ->
              match x with
              | `Nil ->
                nil
              | `Integer i ->
                integer i
              | `String s ->
                string s
              | `Error e ->
                error e
              | `Array a ->
                array a
              | `Bulk (`String s) ->
                bulk_string s
              | `Bulk (`Value s) ->
                bulk_value s )
          |~ case0 "Nil" `Nil
          |~ case1 "Integer" int64 (fun i -> `Integer i)
          |~ case1 "String" string (fun s -> `String s)
          |~ case1 "Error" string (fun s -> `Error s)
          |~ case1 "Array" (array t) (fun x -> `Array x)
          |~ case1 "BulkValue" string (fun x -> `Bulk (`Value x))
          |~ case1 "BulkString" string (fun x -> `Bulk (`String x))
          |> sealv ))

  let merge = Irmin.Merge.(option (default t))
end

module Contents = Qq.Make (Resp_contents)
module Store = Irmin_mirage.Git.Mem.KV (Contents)

module Main (Console : CONSOLE) (Conduit : Conduit_mirage.S) (Pclock: PCLOCK) (KV: Mirage_kv_lwt.RO) = struct
  type qq = {
    repo: Store.repo;
    mutable branch: Store.branch;
    pclock: Pclock.t;
    console: Console.t;
  }

  module Data = struct type data = qq end

  module Server =
    Resp_lwt_mirage.Server.Make (Resp_server.Auth.String) (Data)
      (Resp_lwt_mirage.Bulk.String)

  let get_store ctx =
    Store.of_branch ctx.repo ctx.branch

  let get_q tree k =
    Store.Tree.find tree k >|= function
    | Some x ->
      x
    | None ->
      Qq.empty


  module Info = Irmin_mirage.Info(struct let name = "qq" end)(Pclock)

  let set_q tree k v =
    Store.Tree.add tree k v

  let del_q tree k =
    Store.Tree.remove tree k

  let with_q ctx ?(strategy = `Set) k f =
    get_store ctx >>= fun t ->
    let info = Info.f ctx.pclock "Update queue: %s" (Irmin.Type.to_string Store.key_t k) in
    Store.with_tree_exn t ~info ~strategy k (fun tree ->
      let tree = match tree with
        | Some t -> t
        | None -> Store.Tree.empty
      in
      get_q tree Store.Key.empty >>= fun q ->
      f q >>= function
      | Some q ->
        set_q tree Store.Key.empty q >>= Lwt.return_some
      | None ->
        del_q tree Store.Key.empty >>= Lwt.return_some)

  let make_key resp =
    match Irmin.Type.of_string Store.key_t @@ Resp.to_string_exn resp with
    | Ok key -> key
    | Error (`Msg e) -> failwith ("make_key:" ^ e)

  let push ctx client _ nargs =
    if nargs < 2 then Server.discard_n client 1 >>= fun () -> Server.invalid_arguments client
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
      with_q ctx (make_key key) (fun q ->
        Lwt.return_some @@ Qq.push q ?priority item)
      >>= fun () -> Server.send client (`String "OK")

  let pop ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      let key = make_key key in
      with_q ctx key (fun q ->
        match Qq.pop q with
        | None ->
            Server.send client `Nil >|= fun () ->
            Some q
        | Some ((p, e), q) ->
            Server.send client (`Array [|e; `Integer (Int64.of_int p)|])  >|= fun () ->
            Some q)

  let del ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      let key = make_key key in
      with_q ctx key (fun _ ->
        Lwt.return_none) >>= fun () ->
      Server.ok client

  let length ctx client _ nargs =
    if nargs < 1 then Server.invalid_arguments client
    else
      Server.recv_s client
      >>= fun key ->
      let key = make_key key in
      with_q ctx key (fun q ->
        Server.send client (`Integer (Qq.length q)) >|= fun () ->
        Some q)

  let rec list_store store key acc =
    Store.list store key >>=
    Lwt_list.fold_left_s (fun acc -> function
      | step, `Contents ->
          let key = Store.Key.rcons key step in
          let key = Irmin.Type.to_string Store.key_t key in
          let key = String.sub key 1 (String.length key - 1) in
          Lwt.return @@ (`Bulk (`String key) :: acc)
      | step, `Node ->
        let key = Store.Key.rcons key step  in
        list_store store key [] >|= fun l -> l @ acc) acc

  let list ctx client _ nargs =
    Server.discard_n client nargs
    >>= fun () ->
    get_store ctx >>= fun t ->
    list_store t Store.Key.empty [] >>= fun x ->
    Server.send client (`Array (Array.of_list x))

  let commands =
    [ ("push", push)
    ; ("pop", pop)
    ; ("length", length)
    ; ("list", list)
    ; ("del", del) ]

  let start console conduit pclock kv _nocrypto =
    let tcp = `TCP (Key_gen.port ()) in
    Conduit.with_tls conduit
    >>= fun conduit ->
    (match Key_gen.ssl () with
    | true ->
      let module X509 = Tls_mirage.X509(KV)(Pclock) in
      X509.certificate kv `Default >|= fun cert ->
      let conf: Tls.Config.server = Tls.Config.server ~certificates:(`Single cert) () in
     (`TLS (conf, tcp))
    | false -> Lwt.return tcp) >>= fun tcp ->
    Store.Repo.v (Irmin_mem.config ()) >>= fun repo ->
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
