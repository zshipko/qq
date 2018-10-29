# qq

A message queue for MirageOS using [RESP](https://github.com/zshipko/resp)

## Installation

### Server

```shell
$ make
```

And an executable will be created named `server/qq`

### Client

```shell
$ opam pin add qq-client .
```

This will allow you to create `qq` clients using OCaml.

