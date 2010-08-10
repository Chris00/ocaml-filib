ROOT = ..
include ../Makefile.conf
SRC = $(ROOT)/src
PA_DO = $(shell ocamlfind query pa_do)/pa_do.cmo
PP = camlp4o $(PA_DO) nums.cma $(SRC)/pa_filib.cmo
SOURCES = $(wildcard *.ml *.cpp)
#OCAMLOPT_FLAGS += -verbose

.PHONY: all byte native
all: byte native
byte: $(SOURCES:.ml=.exe)
native: $(subst .cpp,,$(SOURCES:.ml=.com))

# Do not force the package to be installed to compile the tests:
OCAMLINCLUDES = $(SRC)
$(SOURCES:.ml=.exe): $(SRC)/filib.cma
$(SOURCES:.ml=.com): $(SRC)/filib.cmxa

add_filib: add_filib.cpp
	$(CXX) -o $@ $(CXXFLAGS) -I $(FILIB) $^ $(FILIB_LIB)

.PHONY: speed
speed: add.com add_filib
	@echo "- Filib version (C++):"
	@time ./add_filib
	@echo "- OCaml version:"
	@time ./add.com

include ../Makefile.ocaml

clean::
	$(RM) sum