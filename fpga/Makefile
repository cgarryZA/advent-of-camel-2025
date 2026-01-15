INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

DAYS := 01 02 03 04 05 06 07 08 09 10 11 12

PYTHON := python3
CHECK := ./check_answers.py

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
	  $(PYTHON) $(CHECK) $$d || exit 1; \
	done

run-%:
	dune exec run/run_day$*.exe
	$(PYTHON) $(CHECK) $*

.PHONY: default install uninstall reinstall clean run-all
