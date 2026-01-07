INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

DAYS := 01 03 04 05

default:
	dune build

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

reinstall: uninstall install

clean:
	dune clean

run-all:
	@for d in $(DAYS); do \
	  echo "=== Day $$d ==="; \
	  dune exec run/run_day$$d.exe; \
	done

run-%:
	dune exec run/run_day$*.exe

.PHONY: default install uninstall reinstall clean
