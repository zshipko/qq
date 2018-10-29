open Mirage

let port =
  let doc =
    Key.Arg.info ~doc:"TCP port to listen on for incoming connections" ["port"]
  in
  Key.(create "port" Arg.(opt int 1234 doc))

let addr =
  let doc =
    Key.Arg.info ~doc:"Address to listen on for incomming connections" ["addr"]
  in
  Key.(create "addr" Arg.(opt string "0.0.0.0" doc))

let auth =
  let doc =
    Key.Arg.info ~doc:"Authentication password for connecting clients" ["auth"]
  in
  Key.(create "auth" Arg.(opt (some string) None doc))

let main =
  foreign
    ~keys:[Key.abstract port; Key.abstract auth; Key.abstract addr]
    ~packages:[package "resp-lwt-mirage"]
    ~deps:[abstract nocrypto] "Unikernel.Main"
    (console @-> conduit @-> job)

let stack = socket_stackv4 [Ipaddr.V4.any]
let () = register "qq" [main $ default_console $ conduit_direct ~tls:true stack]
