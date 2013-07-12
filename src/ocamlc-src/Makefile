
SRC_I = main.mli compile.mli errors.mli main_args.mli pparse.mli
SRC= compile.ml errors.ml main_args.ml pparse.ml main.ml

CMIS=$(SRC_I:.mli=.cmi)
CMOS=$(SRC:.ml=.cmo)

COMPLIBS= ~/.opam/4.00.1/lib/ocaml/compiler-libs

INC= ocamlcommon.cma ocamlbytecomp.cma
INCLUDES= -I $(COMPLIBS) $(INC)
INCLUDEJS= -I $(COMPLIBS) -I ../toplevel-src/cmicomp \
	../toplevel-src/toplevel_runtime.js js_filemanager.js

JSFLAGS = -pretty -noinline -toplevel
JSPKGS = -package js_of_ocaml,js_of_ocaml.syntax,js_of_ocaml_compiler,js_of_ocaml_compiler-libs \
	 -syntax camlp4o -linkpkg
PP = "camlp4o ~/.opam/4.00.1/lib/js_of_ocaml/pa_js.cmo"

CAMLC=ocamlc
CAMLFIND=ocamlfind
CAMLDEP=ocamldep -pp $(PP)
CAMLJS= $(CAMLFIND) $(CAMLC) $(JSPKGS) $(FLAGS) 

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx .mll .mly

.ml.cmo:
	$(CAMLJS) $(INCLUDES) -c $<

.mli.cmi:
	$(CAMLJS) $(INCLUDES) -c $<


all: depend $(CMIS) ocamlc
	js_of_ocaml $(JSFLAGS) $(INCLUDEJS) ocamlc.byte


ocamlc: $(CMOS)
	$(CAMLJS) $(INCLUDES) $^ -o $@.byte 

depend: $(SRC) $(SRC_I)
	$(CAMLDEP) *.mli *.ml > .depend


clean:
	rm -rf *.o *.byte *.cm[ioax] ocamlc.js

-include .depend
