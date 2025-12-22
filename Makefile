INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

DAY ?= 1

default:
	dune build

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

solve:
	dune exec ./bin/solve_day$(DAY).exe -- inputs/day$(DAY)/input.txt

.PHONY: default install uninstall reinstall clean
