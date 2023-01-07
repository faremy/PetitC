F = "2b"
build:
	dune build
clean:
	dune clean
autotest: build
	cd tests; ./autotest.sh -$(F) ../petitc

machin: build
	./petitc machin.c
	gcc -no-pie machin.s -o machin -g

.PHONY: build clean autotest
