ROOT = ..
include ../Makefile.conf
STUBS_LIB = filib_stubs
DOCDIR = ../doc

INSTALL_FILES = META filib.mli filib.cmi filib.cma \
  filib.cmxa filib.a filib_top.cma \
  filib.cmx $(wildcard dll$(STUBS_LIB).* lib$(STUBS_LIB).*)

.PHONY: all byte native
all: byte native
byte: filib.cma filib_top.cma
native: filib.cmxa

# by default, filib compiles to a static lib which will be included in
# the OCaml lib.
filib.cma filib.cmxa: filib.cmo filib.cmx filib_stubs$(OBJ)
	ocamlmklib -o $(basename $@) -oc $(STUBS_LIB) $^ -lstdc++ \
	  -cclib $(FILIB_LIB) -custom

filib_stubs$(OBJ): filib_stubs.cpp
	$(CXX) $(CXXFLAGS) -I $(FILIB) -c $<

filib_top.cma: filib_top.cmo

ifneq "$(shell  ocamlfind query pa_do 2> /dev/null)" ""
OCAMLPACKS += pa_do
byte: pa_filib.cmo
#PP = camlp4 -parser oq -parser op pr_dump.cmo
INSTALL_FILES += pa_filib.cmo
pa_filib.cmo: OCAMLPACKS += pa_do.num,camlp4.quotations.o
pa_filib.cmo: OCAMLC_FLAGS += -syntax camlp4o
# For ocamldep:
SYNTAX_OPTS += -syntax camlp4o -package "pa_do.num,camlp4.quotations.o"
endif

.PHONY: install uninstall reinstall doc

install: all
	$(OCAMLFIND) install $(PKG_NAME) $(INSTALL_FILES)

uninstall:
	$(OCAMLFIND) remove $(PKG_NAME)

reinstall: $(INSTALL_FILES)
	$(MAKE) uninstall
	$(MAKE) install

doc:
	$(OCAMLDOC) -html -d $(DOCDIR) -colorize-code \
	  $(OCAML_PACKAGES) $(wildcard *.mli)

include ../Makefile.ocaml

clean::
	$(RM) $(wildcard *.so)
	$(RM) $(wildcard $(DOCDIR)/*.html $(DOCDIR)/*.css)
