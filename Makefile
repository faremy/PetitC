F = "3b"
build:
	dune build
clean:
	dune clean
autotest: build
	cd tests; ./autotest.sh -$(F) ../petitc

.PHONY: build clean autotest
