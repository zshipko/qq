BACKEND?=unix

build: server client

.PHONY: server
server:
	cd server && mirage configure -t $(BACKEND) && mirage build

.PHONY: client
client:
	dune build


clean:
	cd server && mirage clean
	dune clean
