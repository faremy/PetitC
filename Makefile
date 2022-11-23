F = "1b"
build:
	dune build
clean:
	dune clean
autotest: build
	cd tests; ./autotest.sh -$(F) ../_build/default/petitc.exe

.PHONY: build clean autotest
