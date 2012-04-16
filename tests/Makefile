ROOT = ..
include $(ROOT)/Makefile.conf
SRC = $(ROOT)/src
#OCAMLPACKS = filib.syntax filib
PA_DO = $(shell $(OCAMLFIND) query pa_do)/pa_do.cmo
PP = camlp4o $(PA_DO) nums.cma $(SRC)/pa_filib.cmo
SOURCES = $(wildcard *.ml *.cpp)
OCAMLC_FLAGS += -verbose -annot
#OCAMLOPT_FLAGS += -syntax camlp4o

.PHONY: all byte native
all: byte native
byte: $(SOURCES:.ml=.exe)
	@echo "**************************************************************"
	@echo "* If you want to execute the bytecode executables (.exe) use *"
	@echo "* LD_LIBRARY_PATH=src/ ./test/<progname>.exe                 *"
	@echo "**************************************************************"
native: $(subst .cpp,,$(SOURCES:.ml=.com))

# Do not force the package to be installed to compile the tests:
OCAMLINCLUDES = $(SRC)
$(SOURCES:.ml=.exe): $(SRC)/filib.cma
$(SOURCES:.ml=.com): $(SRC)/filib.cmxa

%_filib: %_filib.cpp
	$(CXX) -o $@ $(CXXFLAGS) -I $(FILIB) $^ $(FILIB_LIB)

.PHONY: speed
speed: add.com add_filib
	@echo "- Filib version (C++):"
	@time ./add_filib
	@echo "- OCaml version:"
	@time ./add.com

include $(ROOT)/Makefile.ocaml

clean::
	$(RM) sum