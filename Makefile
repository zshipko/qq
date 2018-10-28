BACKEND?=unix

build-server:
	dune build
	cd server && mirage configure -t $(BACKEND) && mirage build
