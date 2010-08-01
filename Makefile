
.PHONY: all byte native install uninstall reinstall doc
all: byte native
byte native install uninstall reinstall doc:
	$(MAKE) -C src $@

.PHONY: tests speed
tests: all
	$(MAKE) -C tests all
tests/%: all
	$(MAKE) -C tests $*
speed:
	$(MAKE) -C tests $@

clean:
	$(MAKE) -C src $@
	$(MAKE) -C tests $@
