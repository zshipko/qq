#[macro_use]
extern crate redis_async as redis;
extern crate futures;
extern crate tokio;

use self::redis::client::PairedConnection;
use self::redis::error::Error;
use self::redis::resp::{FromResp, RespValue, ToRespString};
use futures::{future, sync::oneshot, Async, Future, Poll};

use std::net::SocketAddr;

#[derive(Clone)]
pub struct Client {
    conn: PairedConnection,
}

impl Future for Client {
    type Item = Client;
    type Error = Error;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        Ok(Async::Ready(self.clone()))
    }
}

impl Client {
    pub fn connect(addr: &SocketAddr) -> impl Future<Item = Client, Error = Error> {
        self::redis::client::paired_connect(addr).and_then(|conn| Client { conn: conn })
    }

    pub fn push<T: ToRespString>(
        &self,
        key: &str,
        value: T,
        priority: u32,
    ) -> impl Future<Item = (), Error = Error> {
        self.conn
            .send::<RespValue>(resp_array!(
                "push",
                key,
                value.to_resp_string(),
                format!("{}", priority)
            )).and_then(|_| future::ok(()))
    }

    pub fn pop<T: FromResp>(&self, key: &str) -> impl Future<Item = (T, u32), Error = Error> {
        self.conn.send(resp_array!("pop", key))
    }

    pub fn length(&self, key: &str) -> impl Future<Item = usize, Error = Error> {
        self.conn.send(resp_array!("length", key))
    }

    pub fn list(&self) -> impl Future<Item = Vec<String>, Error = Error> {
        self.conn.send(resp_array!("list"))
    }

    pub fn del(&self, key: &str) -> impl Future<Item = (), Error = Error> {
        self.conn
            .send::<RespValue>(resp_array!("del", key))
            .and_then(|_| future::ok(()))
    }
}

pub fn execute<T: 'static + Send>(
    f: impl Future<Item = T, Error = Error> + Send + 'static,
) -> Result<T, Error> {
    let (c, p) = oneshot::channel();

    let fx = f.then(|result| match result {
        Ok(x) => match c.send(x) {
            Ok(r) => future::ok(r),
            Err(_) => future::err(()),
        },
        Err(_) => future::err(()),
    });

    tokio::run(fx);
    let x = p.wait()?;
    Ok(x)
}

#[test]
fn test_basic() {
    let addr = "10.0.0.2:1234".parse().unwrap();
    let f = Client::connect(&addr).and_then(|client| {
        client
            .push("test", "test", 100)
            .and_then(move |_| client.list())
    });
    match execute(f) {
        Ok(x) => assert_eq!(x, vec!["test"]),
        Err(e) => panic!(e),
    }
}
