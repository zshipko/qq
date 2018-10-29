BACKEND?=unix
prefix?=/usr/local

build: server client

.PHONY: server
server:
	cd server && mirage configure -t $(BACKEND) && mirage build

.PHONY: client
client:
	dune build

install:
	install server/qq $(prefix)/bin

uninstall:
	rm -rf $(prefix)/bin/qq

clean:
	cd server && mirage clean
	dune clean
