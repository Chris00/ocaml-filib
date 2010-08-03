SRC = ../src
PA_DO = $(shell ocamlfind query pa_do)/pa_do.cmo
PP = camlp4o $(PA_DO) $(SRC)/pa_filib.cmo
SOURCES = $(wildcard *.ml)
OCAMLOPT_FLAGS += -verbose

.PHONY: all byte native
all: byte native
byte: $(SOURCES:.ml=.exe)
native: $(SOURCES:.ml=.com)

# Do not force the package to be installed to compile the tests:
OCAMLINCLUDES = $(SRC)
$(SOURCES:.ml=.exe): $(SRC)/filib.cma
$(SOURCES:.ml=.com): $(SRC)/filib.cmxa

.PHONY: speed
speed: add.com
	@echo "- OCaml version (imperative):"
	@time ./add.com

include ../Makefile.ocaml

clean::
	$(RM) sum