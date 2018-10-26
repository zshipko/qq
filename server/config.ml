open Mirage

let port =
  let doc =
    Key.Arg.info ~doc:"TCP port to listen on for incoming connections" ["port"]
  in
  Key.(create "port" Arg.(opt int 1234 doc))

let auth =
  let doc =
    Key.Arg.info ~doc:"Authentication password for connecting clients" ["auth"]
  in
  Key.(create "auth" Arg.(opt (some string) None doc))

let main =
  foreign
    ~keys:[Key.abstract port; Key.abstract auth]
    ~packages:[package "resp-lwt-mirage"]
    ~deps:[abstract nocrypto] "Unikernel.Main"
    (console @-> conduit @-> job)

let stack = socket_stackv4 [Ipaddr.V4.any]

let () =
  register "qq-server" [main $ default_console $ conduit_direct ~tls:true stack]
