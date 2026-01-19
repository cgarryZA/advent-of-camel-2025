INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

DAYS_ALL := 01 02 03 04 05 06 07 08 09 10 11 12
DAYS := $(DAYS_ALL)

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

run-%:
	dune exec run/run_days.exe $*

run:
	@for d in $(DAYS); do \
	  echo "=== Day $$d ==="; \
	  dune exec run/run_days.exe $$d; \
	done

tests:
	@for d in $(DAYS); do \
	  $(PYTHON) $(CHECK) $$d || exit 1; \
	done

tests-%:
	@$(MAKE) tests DAYS=$*

check: run tests

check-%: run-% tests-%

.PHONY: default install uninstall reinstall clean check run tests
